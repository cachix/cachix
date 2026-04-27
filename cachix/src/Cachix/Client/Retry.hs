module Cachix.Client.Retry
  ( -- * Whole-operation retry
    retryAll,

    -- * HTTP retry
    retryHttp,
    retryClientM,

    -- * Policies
    endlessConstantRetryPolicy,
  )
where

import Cachix.Client.Exception (CachixException)
import Control.Concurrent.Async qualified as Async
import Control.Exception.Safe (Handler (..))
import Control.Monad.Catch (MonadCatch, MonadMask, handleJust, throwM)
import Control.Retry
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.List (lookup)
import Data.Text qualified as T
import Data.Time
  ( UTCTime,
    defaultTimeLocale,
    diffUTCTime,
    getCurrentTime,
    nominalDiffTimeToSeconds,
    readPTime,
    rfc822DateFormat,
  )
import GHC.Read (Read (readPrec))
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Media qualified as Media
import Network.HTTP.Types.Header (Header, hContentType, hRetryAfter)
import Network.HTTP.Types.Status qualified as HTTP
import Protolude hiding (Handler (..), handleJust)
import Servant.Client (ClientEnv, ClientError (..))
import Servant.Client qualified as Servant
import Servant.Client.Streaming (ClientM, runClientM)
import Text.ParserCombinators.ReadPrec qualified as ReadPrec (lift)

-- Policies

-- | Per-HTTP-request policy. Wall-time bounded so the user-visible delay is
-- predictable regardless of how many attempts fit. Jittered exponential
-- backoff to avoid thundering-herd on shared origins (CDN incidents).
httpRetryPolicy :: (MonadIO m) => RetryPolicyM m
httpRetryPolicy =
  limitRetriesByCumulativeDelay (secondsToMicros 90) $
    capDelay (secondsToMicros 15) $
      fullJitterBackoff (secondsToMicros 1)

-- | Whole-operation policy used by 'retryAll'. Small attempt count with a
-- short delay, since each attempt re-runs the entire operation (e.g. a full
-- NAR re-upload). Compounds with 'httpRetryPolicy' on HTTP errors, so
-- keep this tight to bound worst-case wall time per store path.
retryAllPolicy :: RetryPolicy
retryAllPolicy =
  constantDelay (secondsToMicros 5) <> limitRetries 3

-- | Constant 1s delay, no attempt cap. For callers that own their own
-- termination condition (e.g. websocket reconnect loops).
endlessConstantRetryPolicy :: RetryPolicy
endlessConstantRetryPolicy =
  constantDelay (secondsToMicros 1)

-- Whole-operation retry

-- | Retry an entire operation on any synchronous exception, except for
-- 'ExitCode' and 'CachixException' (which are fatal). Use for operations
-- that re-run the full unit of work, like a NAR upload.
retryAll :: (MonadIO m, MonadMask m) => (RetryStatus -> m a) -> m a
retryAll f =
  recovering retryAllPolicy handlers $
    rethrowLinkedThreadExceptions . f
  where
    handlers = skipAsyncExceptions ++ [exitCodeHandler, cachixExceptionsHandler, allHandler]

    -- Skip over exitSuccess/exitFailure
    exitCodeHandler _ = Handler $ \(_ :: ExitCode) -> return False

    -- Skip over fatal Cachix exceptions
    cachixExceptionsHandler _ = Handler $ \(_ :: CachixException) -> return False

    -- Retry everything else
    allHandler _ = Handler $ \(_ :: SomeException) -> return True

-- HTTP retry

-- | Retry an HTTP operation that signals failure by throwing
-- 'HTTP.HttpException' or Servant 'ClientError'. Honors @Retry-After@ headers
-- and Cloudflare problem-details @retry_after@ bodies.
--
-- For Servant calls that return @Either ClientError a@ (e.g. 'runClientM'),
-- use 'retryClientM' instead — 'retryHttp' only triggers on thrown errors.
retryHttp :: (MonadIO m, MonadMask m) => m a -> m a
retryHttp = recoveringDynamic httpRetryPolicy handlers . const
  where
    handlers =
      map (fmap toRetryAction .) skipAsyncExceptions
        ++ [ \_ -> Handler httpExceptionToRetryAction,
             \_ -> Handler clientErrorToRetryAction
           ]

-- | Run a Servant client action with HTTP retries.
--
-- 'runClientM' returns 'Either ClientError a' instead of throwing, so wrapping
-- it with 'retryHttp' would never trigger the retry handlers. This inspects
-- the return value directly, sharing the same retry decision logic as
-- 'retryHttp'.
retryClientM :: (MonadIO m, NFData a) => ClientEnv -> ClientM a -> m (Either ClientError a)
retryClientM env req =
  retryingDynamic httpRetryPolicy checkResult $ const $ liftIO (runClientM req env)
  where
    checkResult _ (Right _) = pure DontRetry
    checkResult _ (Left err) = clientErrorToRetryAction err

-- Retry decisions
--
-- Decide whether a given failure is worth retrying, and at what delay. Shared
-- between 'retryHttp' (exception path) and 'retryClientM' (Either path).

-- | Whether an HTTP exception is worth retrying. Temporary connection or
-- network transfer errors are good candidates.
shouldRetryHttpException :: HTTP.HttpException -> Bool
shouldRetryHttpException (HTTP.InvalidUrlException _ _) = False
shouldRetryHttpException (HTTP.HttpExceptionRequest _ reason) =
  case reason of
    HTTP.ConnectionClosed -> True
    HTTP.ConnectionFailure _ -> True
    HTTP.ConnectionTimeout -> True
    HTTP.IncompleteHeaders -> True
    HTTP.InternalException _ -> True
    HTTP.InvalidChunkHeaders -> True
    HTTP.InvalidProxyEnvironmentVariable _ _ -> True
    HTTP.InvalidStatusLine _ -> True
    HTTP.NoResponseDataReceived -> True
    HTTP.ProxyConnectException _ _ status
      | HTTP.statusIsServerError status -> True
    HTTP.ResponseBodyTooShort _ _ -> True
    HTTP.ResponseTimeout -> True
    HTTP.StatusCodeException response _ ->
      shouldRetryHttpStatusCode (HTTP.responseStatus response)
    HTTP.HttpZlibException _ -> True
    _ -> False

-- | Whether an HTTP status code is worth retrying. 429 and any 5xx.
shouldRetryHttpStatusCode :: HTTP.Status -> Bool
shouldRetryHttpStatusCode code
  | code == HTTP.tooManyRequests429 = True
  | HTTP.statusIsServerError code = True
  | otherwise = False

-- | Convert an 'HTTP.HttpException' to a 'RetryAction', honoring any 'Retry-After' hints.
httpExceptionToRetryAction :: (MonadIO m) => HTTP.HttpException -> m RetryAction
httpExceptionToRetryAction ex@(HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response body))
  | shouldRetryHttpException ex =
      let headers = HTTP.responseHeaders response
       in overrideDelayWithRetryAfter $
            lookupRetryAfter headers
              <|> lookupRetryAfterBody headers (LBS.fromStrict body)
httpExceptionToRetryAction ex = pure $ toRetryAction $ shouldRetryHttpException ex

-- | Convert a 'ClientError' to a 'RetryAction', honoring any 'Retry-After' hints.
clientErrorToRetryAction :: (MonadIO m) => ClientError -> m RetryAction
clientErrorToRetryAction (FailureResponse _req res)
  | shouldRetryHttpStatusCode (Servant.responseStatusCode res) =
      let headers = toList (Servant.responseHeaders res)
       in overrideDelayWithRetryAfter $
            lookupRetryAfter headers
              <|> lookupRetryAfterBody headers (Servant.responseBody res)
clientErrorToRetryAction (ConnectionError ex) =
  case fromException ex of
    Just httpException -> httpExceptionToRetryAction httpException
    Nothing -> pure DontRetry
clientErrorToRetryAction _ = pure DontRetry

-- Retry-After
--
-- Parse and honor server hints about when to retry,
data RetryAfter
  = RetryAfterDate UTCTime
  | RetryAfterSeconds Int
  deriving (Eq, Show)

instance Read RetryAfter where
  readPrec = parseSeconds <|> parseWebDate
    where
      parseSeconds = RetryAfterSeconds <$> readPrec
      parseWebDate = ReadPrec.lift $ RetryAfterDate <$> readPTime True defaultTimeLocale rfc822DateFormat

-- | Honor a parsed 'RetryAfter', clamped to 'maxRetryAfterMicros'. Negative
-- or past-dated values fall back to the policy's own delay.
overrideDelayWithRetryAfter :: (MonadIO m) => Maybe RetryAfter -> m RetryAction
overrideDelayWithRetryAfter Nothing = pure ConsultPolicy
overrideDelayWithRetryAfter (Just (RetryAfterSeconds s))
  | s <= 0 = pure ConsultPolicy
  | otherwise = pure $ ConsultPolicyOverrideDelay (clampMicros (secondsToMicros s))
overrideDelayWithRetryAfter (Just (RetryAfterDate date)) = do
  s <- secondsFromNow date
  pure $
    if s > 0
      then ConsultPolicyOverrideDelay (clampMicros (secondsToMicros s))
      else ConsultPolicy
  where
    secondsFromNow d = do
      now <- liftIO getCurrentTime
      pure $ ceiling $ nominalDiffTimeToSeconds (d `diffUTCTime` now)

-- | Bound a delay (in microseconds) by 'maxRetryAfterMicros'.
clampMicros :: Int -> Int
clampMicros micros
  | micros < 0 = maxRetryAfterMicros
  | otherwise = min micros maxRetryAfterMicros

lookupRetryAfter :: [Header] -> Maybe RetryAfter
lookupRetryAfter = readMaybe . T.unpack . decodeUtf8With lenientDecode <=< lookup hRetryAfter

-- | Parse @retry_after@ (in seconds) from an @application/problem+json@ body.
--
-- @retry_after@ is not part of RFC 9457 (which only standardizes @type@,
-- @status@, @title@, @detail@, @instance@), but Cloudflare returns it as an
-- extension member on origin 5xx error pages instead of the canonical
-- 'Retry-After' HTTP header. Honoring it lets us back off the right amount
-- during transient origin outages.
lookupRetryAfterBody :: [Header] -> LBS.ByteString -> Maybe RetryAfter
lookupRetryAfterBody headers body = do
  ct <- lookup hContentType headers
  -- Some origins serve problem-details bodies with the bare @application/json@
  -- content type, so accept that too.
  _ <- Media.matchContent acceptableContentTypes ct
  pd <- Aeson.decode' body
  RetryAfterSeconds <$> problemRetryAfter pd
  where
    acceptableContentTypes =
      ["application" Media.// "problem+json", "application" Media.// "json"]

newtype ProblemDetails = ProblemDetails {problemRetryAfter :: Maybe Int}

instance Aeson.FromJSON ProblemDetails where
  parseJSON = Aeson.withObject "ProblemDetails" $ \o ->
    ProblemDetails <$> o Aeson..:? "retry_after"

-- Time

-- | Convert seconds to microseconds (the unit 'threadDelay' and the @retry@
-- package use for delays).
secondsToMicros :: Int -> Int
secondsToMicros = (* 1_000_000)

-- | Cap on a server-supplied 'Retry-After' override. Bounds the per-attempt
-- sleep so a hostile or buggy origin can't park the client for a long time.
maxRetryAfterMicros :: Int
maxRetryAfterMicros = secondsToMicros 120

-- Linked-thread exception unwrapping

-- | Unwrap 'Async.ExceptionInLinkedThread' and rethrow the inner exception so
-- retry handlers can match on the actual cause.
rethrowLinkedThreadExceptions :: (MonadCatch m) => m a -> m a
rethrowLinkedThreadExceptions =
  handleJust unwrapLinkedThreadException throwM

unwrapLinkedThreadException :: SomeException -> Maybe SomeException
unwrapLinkedThreadException e
  | Just (Async.ExceptionInLinkedThread _ e') <- fromException e = Just e'
  | otherwise = Nothing
