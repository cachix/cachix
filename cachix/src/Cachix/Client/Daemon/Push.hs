module Cachix.Client.Daemon.Push where

import qualified Cachix.API as API
import Cachix.Client.CNix (filterInvalidStorePath, followLinksToStorePath)
import Cachix.Client.Commands.Push hiding (pushStrategy)
import Cachix.Client.Daemon.Event
import Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Types (Daemon, DaemonEnv (..), PushJob (..))
import Cachix.Client.Env (Env (..))
import Cachix.Client.HumanSize (humanSize)
import Cachix.Client.OptionsParser as Client.OptionsParser
  ( PushOptions (..),
  )
import Cachix.Client.Push as Client.Push
import Cachix.Client.Retry (retryAll, retryHttp)
import Cachix.Client.Servant
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Conduit as C
import Control.Exception.Safe (throwM)
import qualified Control.Monad.Catch as E
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Retry (RetryStatus (..))
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Set as Set
import Data.String (String)
import qualified Data.Text as T
import Hercules.CNix (StorePath)
import Hercules.CNix.Store (Store, storePathToPath, withStore)
import qualified Katip
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import System.Console.AsciiProgress
import System.Console.Pretty
import System.IO (hIsTerminalDevice)
import qualified UnliftIO.Async as Async
import qualified UnliftIO.QSem as QSem

withPushParams :: ((PushJob -> Daemon (PushParams Daemon ())) -> Daemon b) -> Daemon b
withPushParams m = do
  DaemonEnv {..} <- ask
  let authToken = getAuthTokenFromPushSecret daemonPushSecret
      cacheName = BinaryCache.name daemonBinaryCache
      compressionMethod = getCompressionMethod daemonPushOptions daemonBinaryCache

  withStore $ \store ->
    m $
      \pushJob -> do
        pushStrategy <- newPushStrategy store authToken daemonPushOptions cacheName compressionMethod pushJob

        return $
          PushParams
            { pushParamsName = cacheName,
              pushParamsSecret = daemonPushSecret,
              pushParamsClientEnv = clientenv daemonEnv,
              pushOnClosureAttempt = \full missing -> do
                let already = Set.toList $ Set.difference (Set.fromList full) (Set.fromList missing)
                mapM_ (onAlreadyPresent . pushStrategy) already
                return missing,
              pushParamsStrategy = pushStrategy,
              pushParamsStore = store
            }

newPushStrategy :: Store -> Maybe Token -> PushOptions -> Text -> BinaryCache.CompressionMethod -> PushJob -> Daemon (StorePath -> PushStrategy Daemon ())
newPushStrategy store authToken opts cacheName compressionMethod pushJob = do
  -- TODO: fetch everything here?
  return $ \storePath -> do
    PushStrategy
      { onAlreadyPresent = do
          sp <- liftIO $ storePathToPath store storePath
          pushStorePathDone (pushId pushJob) (toS sp)
          Katip.logFM Katip.InfoS $ Katip.ls $ "Skipping " <> (toS sp :: Text),
        on401 = liftIO . handleCacheResponse cacheName authToken,
        onError = throwM,
        onAttempt = \_retryStatus size -> do
          sp <- liftIO $ storePathToPath store storePath
          pushStorePathAttempt (pushId pushJob) (toS sp) size
          Katip.logFM Katip.InfoS $ Katip.ls $ "Pushing " <> (toS sp :: Text),
        onUncompressedNARStream = \_ size -> do
          sp <- liftIO $ storePathToPath store storePath
          lastPush <- liftIO $ newIORef (0 :: Int64)
          totalCount <- liftIO $ newIORef (0 :: Int64)
          C.awaitForever $ \chunk -> do
            C.yield chunk
            let byteCount = fromIntegral $ BS.length chunk
            newTotalCount <- liftIO $ atomicModifyIORef' totalCount (\b -> (b + byteCount, b + byteCount))
            lastCount <- liftIO $ readIORef lastPush
            when (newTotalCount - lastCount > 1024 || newTotalCount == size) $ do
              liftIO $ writeIORef lastPush newTotalCount
              lift $ lift $ pushStorePathProgress (pushId pushJob) (toS sp) newTotalCount,
        onDone = do
          sp <- liftIO $ storePathToPath store storePath
          pushStorePathDone (pushId pushJob) (toS sp)
          Katip.logFM Katip.InfoS $ Katip.ls $ "Pushed " <> (toS sp :: Text),
        Client.Push.compressionMethod = compressionMethod,
        Client.Push.compressionLevel = Client.OptionsParser.compressionLevel opts,
        Client.Push.omitDeriver = Client.OptionsParser.omitDeriver opts
      }

-- TODO: split into two jobs: 1. query/normalize/filter 2. push store path
handleRequest :: (PushJob -> Daemon (PushParams Daemon a)) -> PushJob -> Daemon ()
handleRequest getPushParams pushJob@(PushJob {..}) = do
  pushParams <- getPushParams pushJob
  let store = pushParamsStore pushParams
  normalized <- mapM (normalizeStorePath store) (Protocol.storePaths pushRequest)

  (allPaths, missingPaths) <- getMissingPathsForClosure pushParams (catMaybes normalized)

  paths <- pushOnClosureAttempt pushParams allPaths missingPaths

  qs <- asks daemonPushSemaphore
  let upload storePath =
        E.bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) $
          retryAll $
            uploadStorePath pushParams storePath

  Async.mapConcurrently_ upload paths

getBinaryCache :: Env -> Maybe Token -> BinaryCacheName -> IO BinaryCache.BinaryCache
getBinaryCache env authToken name = do
  -- Self-signed caches might not have a token, which is why this code is so weird.
  -- In practice, public self-signed caches don't need one and private ones always need a token.
  let token = fromMaybe (Token "") authToken
  res <- retryHttp $ (`runClientM` clientenv env) $ API.getCache cachixClient token name
  case res of
    Left err -> handleCacheResponse name authToken err
    Right binaryCache -> pure binaryCache

getCompressionMethod :: PushOptions -> BinaryCache.BinaryCache -> BinaryCache.CompressionMethod
getCompressionMethod opts binaryCache =
  fromMaybe BinaryCache.ZSTD $
    Client.OptionsParser.compressionMethod opts
      <|> Just (BinaryCache.preferredCompressionMethod binaryCache)

normalizeStorePath :: (MonadIO m) => Store -> FilePath -> m (Maybe StorePath)
normalizeStorePath store fp =
  liftIO $ runMaybeT $ do
    storePath <- MaybeT $ followLinksToStorePath store (encodeUtf8 $ T.pack fp)
    MaybeT $ filterInvalidStorePath store storePath

showUploadProgress :: String -> Int64 -> IO (Int64 -> IO ())
showUploadProgress path size = do
  isTerminal <- liftIO $ hIsTerminalDevice stdout
  if isTerminal
    then uploadProgress path size
    else fallbackUploadProgress path size

completedUpload :: String -> String -> String
completedUpload path hSize =
  color Green "✓ " <> path <> " (" <> hSize <> ")"

uploadProgress :: String -> Int64 -> IO (Int64 -> IO ())
uploadProgress path size = do
  let hSize = toS $ humanSize $ fromIntegral size
  let bar = color Blue "[:bar] " <> toS path <> " (:percent of " <> hSize <> ")"
      barLength = T.length $ T.replace ":percent" "  0%" (T.replace "[:bar]" "" (toS bar))

  progressBar <-
    liftIO $
      newProgressBar
        def
          { pgTotal = fromIntegral size,
            -- https://github.com/yamadapc/haskell-ascii-progress/issues/24
            pgWidth = 20 + barLength,
            pgOnCompletion = Just $ completedUpload path hSize,
            pgFormat = bar
          }

  lastUpdateRef <- liftIO $ newIORef (0 :: Int64)

  return $ \progress -> do
    lastUpdate <- readIORef lastUpdateRef
    writeIORef lastUpdateRef progress
    let deltaBytes = fromIntegral (progress - lastUpdate)
    liftIO $ tickN progressBar deltaBytes

retryText :: RetryStatus -> Text
retryText retryStatus =
  if rsIterNumber retryStatus == 0
    then ""
    else color Yellow $ "retry #" <> show (rsIterNumber retryStatus) <> " "

fallbackUploadProgress :: String -> Int64 -> IO (Int64 -> IO ())
fallbackUploadProgress path size = do
  let hSize = toS $ humanSize $ fromIntegral size
  -- we append newline instead of putStrLn due to https://github.com/haskell/text/issues/242
  -- appendErrText $ retryText retryStatus <> "Pushing " <> path <> " (" <> toS hSize <> ")\n"
  hPutStr stderr $ "Pushing " <> path <> " (" <> hSize <> ")\n"
  return $ const pass
