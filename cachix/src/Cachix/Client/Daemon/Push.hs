module Cachix.Client.Daemon.Push where

import qualified Cachix.API as API
import Cachix.Client.CNix (filterInvalidStorePath, followLinksToStorePath)
import Cachix.Client.Commands.Push hiding (pushStrategy)
import Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.PushManager
import Cachix.Client.Daemon.Types (Daemon, DaemonEnv (..), PushJob (..), PushManager)
import Cachix.Client.Env (Env (..))
import Cachix.Client.OptionsParser as Client.OptionsParser
  ( PushOptions (..),
  )
import Cachix.Client.Push as Client.Push
import Cachix.Client.Retry (retryAll, retryHttp)
import Cachix.Client.Servant
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Conduit as C
import qualified Control.Monad.Catch as E
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Set as Set
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
import qualified UnliftIO.Async as Async
import qualified UnliftIO.QSem as QSem

withPushParams :: (PushParams PushManager () -> Daemon b) -> Daemon b
withPushParams m = do
  DaemonEnv {..} <- ask
  let authToken = getAuthTokenFromPushSecret daemonPushSecret
      cacheName = BinaryCache.name daemonBinaryCache
      compressionMethod = getCompressionMethod daemonPushOptions daemonBinaryCache

  withStore $ \store ->
    m $ do
      let pushStrategy = newPushStrategy store authToken daemonPushOptions cacheName compressionMethod

      PushParams
        { pushParamsName = cacheName,
          pushParamsSecret = daemonPushSecret,
          pushParamsClientEnv = clientenv daemonEnv,
          pushOnClosureAttempt = \full missing -> do
            let already = Set.toList $ Set.difference (Set.fromList full) (Set.fromList missing)
            mapM_ (onAlreadyPresent . pushStrategy) already
            return full,
          pushParamsStrategy = pushStrategy,
          pushParamsStore = store
        }

newPushStrategy ::
  Store ->
  Maybe Token ->
  PushOptions ->
  Text ->
  BinaryCache.CompressionMethod ->
  (StorePath -> PushStrategy PushManager ())
newPushStrategy store authToken opts cacheName compressionMethod storePath =
  let onAlreadyPresent = do
        sp <- liftIO $ storePathToPath store storePath
        Katip.logFM Katip.InfoS $ Katip.ls $ "Skipping " <> (toS sp :: Text)
        -- TODO: needs another event type here
        pushStorePathDone (toS sp)

      onError err = do
        let errText = toS (displayException err)
        sp <- liftIO $ storePathToPath store storePath
        Katip.katipAddContext (Katip.sl "error" errText) $
          Katip.logFM Katip.InfoS $
            Katip.ls $
              "Failed " <> (toS sp :: Text)
        pushStorePathFailed (toS sp) errText

      onAttempt retryStatus size = do
        sp <- liftIO $ storePathToPath store storePath
        Katip.logFM Katip.InfoS $ Katip.ls $ "Pushing " <> (toS sp :: Text)
        pushStorePathAttempt (toS sp) size retryStatus

      onUncompressedNARStream _ size = do
        sp <- liftIO $ storePathToPath store storePath
        lastEmitRef <- liftIO $ newIORef (0 :: Int64)
        currentBytesRef <- liftIO $ newIORef (0 :: Int64)
        C.awaitForever $ \chunk -> do
          let newBytes = fromIntegral (BS.length chunk)
          currentBytes <- liftIO $ atomicModifyIORef' currentBytesRef (\b -> (b + newBytes, b + newBytes))
          lastEmit <- liftIO $ readIORef lastEmitRef

          when (currentBytes - lastEmit >= 1024 || currentBytes == size) $ do
            liftIO $ writeIORef lastEmitRef currentBytes
            lift $ lift $ pushStorePathProgress (toS sp) currentBytes newBytes

          C.yield chunk

      onDone = do
        sp <- liftIO $ storePathToPath store storePath
        Katip.logFM Katip.InfoS $ Katip.ls $ "Pushed " <> (toS sp :: Text)
        pushStorePathDone (toS sp)
   in PushStrategy
        { onAlreadyPresent = onAlreadyPresent,
          on401 = liftIO . handleCacheResponse cacheName authToken,
          onError = onError,
          onAttempt = onAttempt,
          onUncompressedNARStream = onUncompressedNARStream,
          onDone = onDone,
          Client.Push.compressionMethod = compressionMethod,
          Client.Push.compressionLevel = Client.OptionsParser.compressionLevel opts,
          Client.Push.omitDeriver = Client.OptionsParser.omitDeriver opts
        }

-- TODO: split into two jobs: 1. query/normalize/filter 2. push store path
handleRequest :: PushParams PushManager a -> PushJob -> Daemon ()
handleRequest pushParams pushJob@(PushJob {..}) = do
  qs <- asks daemonPushSemaphore
  pushManager <- asks daemonPushManager

  liftIO $ runPushManager pushManager $ do
    E.bracket_ (pushStarted pushId) (pushFinished pushId) $ do
      let store = pushParamsStore pushParams
      normalized <- mapM (normalizeStorePath store) (Protocol.storePaths pushRequest)
      print normalized

      (allPaths, missingPaths) <- getMissingPathsForClosure pushParams (catMaybes normalized)

      paths <- pushOnClosureAttempt pushParams allPaths missingPaths

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
