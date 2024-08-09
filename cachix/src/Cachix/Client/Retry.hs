module Cachix.Client.Retry
  ( retryAll,
    retryAllWithPolicy,
    retryHttp,
    retryHttpWith,
    endlessRetryPolicy,
    endlessConstantRetryPolicy,
  )
where

import Cachix.Client.Exception (CachixException (..))
import Control.Concurrent.Async qualified as Async
import Control.Exception.Safe (Handler (..))
import Control.Monad.Catch (MonadCatch, MonadMask, handleJust, throwM)
import Control.Retry
import Data.List (lookup)
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
import Network.HTTP.Types.Header (hRetryAfter)
import Network.HTTP.Types.Status qualified as HTTP
import Protolude hiding (Handler (..), handleJust)
import Servant.Client (ClientError (..))
import Servant.Client qualified as Servant
import Text.ParserCombinators.ReadPrec qualified as ReadPrec (lift)

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  exponentialBackoff (1000 * 1000) <> limitRetries 5

endlessConstantRetryPolicy :: RetryPolicy
endlessConstantRetryPolicy =
  constantDelay (1000 * 1000)

endlessRetryPolicy :: RetryPolicy
endlessRetryPolicy =
  exponentialBackoff (1000 * 1000)

retryAll :: (MonadIO m, MonadMask m) => (RetryStatus -> m a) -> m a
retryAll = retryAllWithPolicy defaultRetryPolicy

retryAllWithPolicy :: (MonadIO m, MonadMask m) => RetryPolicyM m -> (RetryStatus -> m a) -> m a
retryAllWithPolicy policy f =
  recovering policy handlers $
    rethrowLinkedThreadExceptions . f
  where
    handlers = skipAsyncExceptions ++ [exitCodeHandler, cachixExceptionsHandler, allHandler]

    -- Skip over exitSuccess/exitFailure
    exitCodeHandler _ = Handler $ \(_ :: ExitCode) -> return False

    -- Skip over fatal Cachix exceptions
    cachixExceptionsHandler _ = Handler $ \(_ :: CachixException) -> return False

    -- Retry everything else
    allHandler _ = Handler $ \(_ :: SomeException) -> return True

-- | Unwrap 'Async.ExceptionInLinkedThread' exceptions and rethrow the inner exception.
rethrowLinkedThreadExceptions :: (MonadCatch m) => m a -> m a
rethrowLinkedThreadExceptions =
  handleJust unwrapLinkedThreadException throwM

unwrapLinkedThreadException :: SomeException -> Maybe SomeException
unwrapLinkedThreadException e
  | Just (Async.ExceptionInLinkedThread _ e') <- fromException e = Just e'
  | otherwise = Nothing

retryHttp :: (MonadIO m, MonadMask m) => m a -> m a
retryHttp = retryHttpWith defaultRetryPolicy

-- | Retry policy for HTTP requests.
--
-- Retries a subset of HTTP exceptions and overrides the delay with the Retry-After header if present.
retryHttpWith :: forall m a. (MonadIO m, MonadMask m) => RetryPolicyM m -> m a -> m a
retryHttpWith policy = recoveringDynamic policy handlers . const
  where
    handlers :: [RetryStatus -> Handler m RetryAction]
    handlers =
      skipAsyncExceptions' ++ [retryHttpExceptions, retryClientExceptions]

    skipAsyncExceptions' = map (fmap toRetryAction .) skipAsyncExceptions

    retryHttpExceptions _ = Handler httpExceptionToRetryHandler
    retryClientExceptions _ = Handler clientExceptionToRetryHandler

    httpExceptionToRetryHandler :: HTTP.HttpException -> m RetryAction
    httpExceptionToRetryHandler (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException response _))
      | statusMayHaveRetryHeader (HTTP.responseStatus response) = overrideDelayWithRetryAfter response
    httpExceptionToRetryHandler ex = return . toRetryAction . shouldRetryHttpException $ ex

    clientExceptionToRetryHandler :: ClientError -> m RetryAction
    clientExceptionToRetryHandler (FailureResponse _req res)
      | shouldRetryHttpStatusCode (Servant.responseStatusCode res) =
          return ConsultPolicy
    clientExceptionToRetryHandler (ConnectionError ex) =
      case fromException ex of
        Just httpException -> httpExceptionToRetryHandler httpException
        Nothing -> return DontRetry
    clientExceptionToRetryHandler _ = return DontRetry

data RetryAfter
  = RetryAfterDate UTCTime
  | RetryAfterSeconds Int
  deriving (Eq, Show)

instance Read RetryAfter where
  readPrec = parseSeconds <|> parseWebDate
    where
      parseSeconds = RetryAfterSeconds <$> readPrec
      parseWebDate = ReadPrec.lift $ RetryAfterDate <$> readPTime True defaultTimeLocale rfc822DateFormat

overrideDelayWithRetryAfter :: (MonadIO m) => HTTP.Response a -> m RetryAction
overrideDelayWithRetryAfter response =
  case lookupRetryAfter response of
    Nothing ->
      return ConsultPolicy
    Just (RetryAfterSeconds seconds) ->
      return $ ConsultPolicyOverrideDelay (seconds * 1000 * 1000)
    Just (RetryAfterDate date) -> do
      seconds <- secondsFromNow date
      return $
        if seconds > 0
          then ConsultPolicyOverrideDelay (seconds * 1000 * 1000)
          else ConsultPolicy
  where
    secondsFromNow date = do
      now <- liftIO getCurrentTime
      return $ ceiling $ nominalDiffTimeToSeconds (date `diffUTCTime` now)

    lookupRetryAfter = readMaybe . decodeUtf8 <=< lookup hRetryAfter . HTTP.responseHeaders

-- | Determine whether the HTTP exception is worth retrying.
--
-- Temporary connection or network transfer errors are good candidates.
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

-- | Determine whether the HTTP status code is worth retrying.
shouldRetryHttpStatusCode :: HTTP.Status -> Bool
shouldRetryHttpStatusCode code | code == HTTP.tooManyRequests429 = True
shouldRetryHttpStatusCode code | HTTP.statusIsServerError code = True
shouldRetryHttpStatusCode _ = False

statusMayHaveRetryHeader :: HTTP.Status -> Bool
statusMayHaveRetryHeader = flip elem [HTTP.tooManyRequests429, HTTP.serviceUnavailable503]
