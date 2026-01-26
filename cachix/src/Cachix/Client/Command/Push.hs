{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.Command.Push
  ( push,
    pushStrategy,
    withPushParams,
    withPushParams',
    handleCacheResponse,
    getPushSecret,
    getPushSecretRequired,
  )
where

import Cachix.API qualified as API
import Cachix.Client.CNix (logStorePathWarning, resolveStorePaths)
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.HumanSize (humanSize)
import Cachix.Client.OptionsParser as Options (PushOptions (..))
import Cachix.Client.Push as Push
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Secrets
import Cachix.Client.Servant
import Cachix.Types.BinaryCache (BinaryCacheName)
import Cachix.Types.BinaryCache qualified as BinaryCache
import Control.Exception.Safe (throwM)
import Control.Retry (RetryStatus (rsIterNumber))
import Data.ByteString qualified as BS
import Data.Conduit qualified as Conduit
import Data.String.Here
import Data.Text qualified as T
import Hercules.CNix (StorePath)
import Hercules.CNix.Store (Store, storePathToPath, withStore)
import Network.HTTP.Types (status401, status404)
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import System.Console.AsciiProgress
import System.Console.Pretty
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice)

push :: Env -> PushOptions -> BinaryCacheName -> [Text] -> IO ()
push env opts name cliPaths = do
  hasStdin <- not <$> hIsTerminalDevice stdin
  inputStorePaths <-
    case (hasStdin, cliPaths) of
      (False, []) -> throwIO $ NoInput "You need to specify store paths either as stdin or as an command argument"
      (True, []) -> T.words <$> getContents
      -- If we get both stdin and cli args, prefer cli args.
      -- This avoids hangs in cases where stdin is non-interactive but unused by caller
      -- some programming environments always create a (non-interactive) stdin
      -- that may or may not be written to by the caller.
      -- This is somewhat like the behavior of `cat` for example.
      (_, paths) -> return paths
  withPushParams env opts name $ \pushParams -> do
    (errors, validPaths) <- liftIO $ resolveStorePaths (pushParamsStore pushParams) (map toS inputStorePaths)
    liftIO $ forM_ errors $ uncurry logStorePathWarning
    pushedPaths <-
      pushClosure
        (mapConcurrentlyBounded (numJobs opts))
        pushParams
        validPaths
    case (length inputStorePaths, length pushedPaths) of
      (0, _) -> putErrText "Nothing to push."
      (_, 0) -> putErrText "Nothing to push - all store paths are already on Cachix."
      _ -> putErrText "\nAll done."

pushStrategy :: Store -> Maybe Token -> PushOptions -> Text -> BinaryCache.CompressionMethod -> StorePath -> PushStrategy IO ()
pushStrategy store authToken opts name compressionMethod storePath =
  PushStrategy
    { onAlreadyPresent = pass,
      on401 = handleCacheResponse name authToken,
      onError = throwM,
      onAttempt = \_ _ -> pass,
      onUncompressedNARStream = showUploadProgress,
      onDone = pass,
      Push.compressionMethod = compressionMethod,
      Push.compressionLevel = Options.compressionLevel opts,
      Push.chunkSize = Options.chunkSize opts,
      Push.numConcurrentChunks = Options.numConcurrentChunks opts,
      Push.omitDeriver = Options.omitDeriver opts
    }
  where
    retryText :: RetryStatus -> Text
    retryText retryStatus =
      if rsIterNumber retryStatus == 0
        then ""
        else color Yellow $ "retry #" <> show (rsIterNumber retryStatus) <> " "

    showUploadProgress retryStatus size = do
      let hSize = toS $ humanSize $ fromIntegral size
      path <- liftIO $ decodeUtf8With lenientDecode <$> storePathToPath store storePath

      isTerminal <- liftIO $ hIsTerminalDevice stderr
      isCI <- liftIO $ (== Just "true") <$> lookupEnv "CI"
      onTick <-
        if isTerminal && not isCI
          then do
            let bar = color Blue "[:bar] " <> toS (retryText retryStatus) <> toS path <> " (:percent of " <> hSize <> ")"
                barLength = T.length $ T.replace ":percent" "  0%" (T.replace "[:bar]" "" (toS bar))

            progressBar <-
              liftIO $
                newProgressBar
                  def
                    { pgTotal = fromIntegral size,
                      -- https://github.com/yamadapc/haskell-ascii-progress/issues/24
                      pgWidth = 20 + barLength,
                      pgOnCompletion = Just $ color Green "✓ " <> toS path <> " (" <> hSize <> ")",
                      pgFormat = bar
                    }

            return $ liftIO . tickN progressBar . BS.length
          else do
            -- we append newline instead of putStrLn due to https://github.com/haskell/text/issues/242
            appendErrText $ retryText retryStatus <> "Pushing " <> path <> " (" <> toS hSize <> ")\n"
            return $ const pass

      Conduit.awaitForever $ \chunk -> do
        Conduit.yield chunk
        onTick chunk

withPushParams :: Env -> PushOptions -> BinaryCacheName -> (PushParams IO () -> IO ()) -> IO ()
withPushParams env pushOpts name m = do
  pushSecret <- getPushSecretRequired (config env) name
  withPushParams' env pushOpts name pushSecret m

withPushParams' :: Env -> PushOptions -> BinaryCacheName -> PushSecret -> (PushParams IO () -> IO ()) -> IO ()
withPushParams' env pushOpts name pushSecret m = do
  let authToken = getAuthTokenFromPushSecret pushSecret

  compressionMethodBackend <- case pushSecret of
    PushSigningKey {} -> pure Nothing
    PushToken token -> do
      res <- retryHttp $ (`runClientM` clientenv env) $ API.getCache cachixClient token name
      case res of
        Left err -> handleCacheResponse name authToken err
        Right binaryCache -> pure (Just $ BinaryCache.preferredCompressionMethod binaryCache)
  let compressionMethod =
        fromMaybe BinaryCache.ZSTD (head $ catMaybes [Options.compressionMethod pushOpts, compressionMethodBackend])

  withStore $ \store ->
    m
      PushParams
        { pushParamsName = name,
          pushParamsSecret = pushSecret,
          pushParamsClientEnv = clientenv env,
          pushOnClosureAttempt = \full missing -> do
            unless (null missing) $ do
              let numMissing = length missing
                  numCached = length full - numMissing
              putErrText $ "Pushing " <> show numMissing <> " paths (" <> show numCached <> " are already present) using " <> T.toLower (show compressionMethod) <> " to cache " <> name <> " ⏳\n"
            return missing,
          pushParamsStrategy = pushStrategy store authToken pushOpts name compressionMethod,
          pushParamsStore = store
        }

handleCacheResponse :: Text -> Maybe Token -> ClientError -> IO a
handleCacheResponse name optionalAuthToken err
  | isErr err status401 && isJust optionalAuthToken = throwM $ accessDeniedBinaryCache name (failureResponseBody err)
  | isErr err status401 = throwM $ notAuthenticatedBinaryCache name
  | isErr err status404 = throwM $ BinaryCacheNotFound $ "Binary cache " <> name <> " does not exist."
  | otherwise = throwM err

failureResponseBody :: ClientError -> Maybe ByteString
failureResponseBody (FailureResponse _ response) = Just $ toS $ responseBody response
failureResponseBody _ = Nothing

notAuthenticatedBinaryCache :: Text -> CachixException
notAuthenticatedBinaryCache name =
  AccessDeniedBinaryCache $
    "Binary cache " <> name <> " doesn't exist or it's private and you need a token: " <> Config.noAuthTokenError

accessDeniedBinaryCache :: Text -> Maybe ByteString -> CachixException
accessDeniedBinaryCache name maybeBody =
  AccessDeniedBinaryCache $ "Binary cache " <> name <> " doesn't exist or you don't have access." <> context maybeBody
  where
    context Nothing = ""
    context (Just body) = " Error: " <> toS body

-- | Fetch the push credentials from the environment or config.
getPushSecret ::
  Config.Config ->
  -- | Cache name
  Text ->
  IO (Either Text PushSecret)
getPushSecret config name = do
  maybeAuthToken <- Config.getAuthTokenMaybe config

  maybeSigningKeyEnv <- toS <<$>> lookupEnv "CACHIX_SIGNING_KEY"
  let maybeSigningKeyConfig = Config.secretKey <$> head (getBinaryCache config)

  case maybeSigningKeyEnv <|> maybeSigningKeyConfig of
    Just signingKey ->
      return $ PushSigningKey (fromMaybe (Token "") maybeAuthToken) <$> parseSigningKeyLenient signingKey
    Nothing -> case maybeAuthToken of
      Just authToken -> return $ Right $ PushToken authToken
      Nothing -> return $ Left msg
  where
    -- we reverse list of caches to prioritize keys added as last
    getBinaryCache c =
      reverse $
        filter (\bc -> Config.name bc == name) (Config.binaryCaches c)

    msg :: Text
    msg =
      [iTrim|
Neither auth token nor signing key are present.

They are looked up via $CACHIX_AUTH_TOKEN and $CACHIX_SIGNING_KEY,
and if missing also looked up from ~/.config/cachix/cachix.dhall

Read https://mycache.cachix.org for instructions how to push to your binary cache.
    |]

-- | Like 'getPushSecret', but throws a fatal error if the secret is not found.
getPushSecretRequired ::
  Config.Config ->
  -- | Cache name
  Text ->
  -- | Secret key or exception
  IO PushSecret
getPushSecretRequired config name = do
  epushSecret <- getPushSecret config name
  case epushSecret of
    -- TODO: technically, we're missing any credentials, not just the signing key
    Left err -> throwIO $ NoSigningKey err
    Right pushSecret -> return pushSecret

-- | Put text to stderr without a new line.
--
-- This is safe to use when printing from multiple threads, unlike hPutStrLn which may fail to insert the newline at the right place.
appendErrText :: (MonadIO m) => Text -> m ()
appendErrText = hPutStr stderr
