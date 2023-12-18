module Cachix.Client.Daemon.PushManager where

import Cachix.Client.CNix (filterInvalidStorePath, followLinksToStorePath)
import Cachix.Client.Commands.Push hiding (pushStrategy)
import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Types.Log (Logger)
import Cachix.Client.Daemon.Types.PushEvent
import Cachix.Client.Daemon.Types.PushManager
import Cachix.Client.OptionsParser as Client.OptionsParser
  ( PushOptions (..),
  )
import Cachix.Client.Push as Client.Push
import Cachix.Client.Retry (retryAll)
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Conduit as C
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Retry (RetryStatus)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Hercules.CNix (StorePath)
import Hercules.CNix.Store (Store, parseStorePath, storePathToPath)
import qualified Katip
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Conduit ()
import qualified UnliftIO.QSem as QSem

newPushManagerEnv :: Logger -> OnPushEvent -> IO PushManagerEnv
newPushManagerEnv pmLogger pmOnPushEvent = do
  pmPushJobs <- newTVarIO mempty
  pmStorePathToPushIds <- newTVarIO mempty
  pmActiveStorePaths <- newTVarIO mempty
  pmTaskQueue <- newTBMQueueIO 1000
  return $ PushManagerEnv {..}

runPushManager :: PushManagerEnv -> PushManager a -> IO a
runPushManager env f = unPushManager f `runReaderT` env

stopPushManager :: PushManagerEnv -> IO ()
stopPushManager PushManagerEnv {pmTaskQueue} =
  atomically $ closeTBMQueue pmTaskQueue

-- Manage push jobs

addPushJob :: (Katip.KatipContext m) => PushManagerEnv -> Protocol.PushRequest -> m ()
addPushJob PushManagerEnv {..} pushRequest = do
  pushJob <- newPushJob pushRequest

  liftIO $
    atomically $ do
      modifyTVar' pmPushJobs $ Map.insert (pushId pushJob) pushJob
      writeTBMQueue pmTaskQueue $ ResolveClosure (pushId pushJob)

  Katip.logLocM Katip.DebugS $ Katip.ls $ "Queued push job " <> (show (pushId pushJob) :: Text)

lookupPushJob :: (MonadIO m) => PushManagerEnv -> Protocol.PushRequestId -> m (Maybe PushJob)
lookupPushJob PushManagerEnv {..} pushId = do
  liftIO $ Map.lookup pushId <$> readTVarIO pmPushJobs

-- Manage store paths

queueStorePath :: (Katip.KatipContext m) => PushManagerEnv -> FilePath -> m ()
queueStorePath PushManagerEnv {..} storePath = do
  mpath <- liftIO $ atomically $ do
    isDuplicate <- Set.member storePath <$> readTVar pmActiveStorePaths

    if isDuplicate
      then pure Nothing
      else do
        writeTBMQueue pmTaskQueue (PushStorePath storePath)
        modifyTVar' pmActiveStorePaths $ Set.insert storePath
        pure $ Just storePath

  for_ mpath $ \fp ->
    Katip.logLocM Katip.DebugS $ Katip.ls $ "Queued store path " <> fp

removeStorePath :: FilePath -> PushManager ()
removeStorePath storePath = do
  pmActiveStorePaths <- asks pmActiveStorePaths
  pmStorePathToPushIds <- asks pmStorePathToPushIds
  liftIO $ atomically $ do
    modifyTVar' pmActiveStorePaths $ Set.delete storePath
    modifyTVar' pmStorePathToPushIds $ Map.delete storePath

lookupPushIdsForStorePath :: FilePath -> PushManager [Protocol.PushRequestId]
lookupPushIdsForStorePath storePath = do
  pmStorePathToPushIds <- asks pmStorePathToPushIds
  pushIdStore <- liftIO $ readTVarIO pmStorePathToPushIds
  pure $ fromMaybe [] $ Map.lookup storePath pushIdStore

handleTask :: PushManagerEnv -> PushParams PushManager () -> Task -> IO ()
handleTask pushManager pushParams task =
  runPushManager pushManager $ do
    case task of
      ResolveClosure pushId -> do
        Katip.logLocM Katip.InfoS $ Katip.ls $ "Resolving closure for push job " <> (show pushId :: Text)

        let store = pushParamsStore pushParams
        mpushJob <- lookupPushJob pushManager pushId

        case mpushJob of
          Nothing ->
            Katip.logLocM Katip.WarningS $ Katip.ls $ "Push job " <> (show pushId :: Text) <> " not found"
          Just pushJob -> do
            let sps = Protocol.storePaths (pushRequest pushJob)
            normalized <- mapM (normalizeStorePath store) sps
            (allPaths, missingPaths) <- getMissingPathsForClosure pushParams (catMaybes normalized)
            Katip.logLocM Katip.DebugS $ Katip.ls $ showResolveStats pushId allPaths missingPaths
            storePaths <- pushOnClosureAttempt pushParams allPaths missingPaths
            forM_ storePaths $ \storePath -> do
              fp <- liftIO $ storePathToPath store storePath
              queueStorePath pushManager (toS fp)
      PushStorePath filePath -> do
        Katip.logLocM Katip.DebugS $ Katip.ls $ "Pushing store path " <> filePath

        -- qs <- asks daemonPushSemaphore
        let store = pushParamsStore pushParams

        storePath <- liftIO $ parseStorePath store (toS filePath)

        -- E.bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) $
        retryAll $ uploadStorePath pushParams storePath
  where
    showResolveStats :: Protocol.PushRequestId -> [StorePath] -> [StorePath] -> Text
    showResolveStats pushId allPaths missingPaths =
      T.intercalate
        "\n"
        [ "Resolved push job " <> show pushId,
          "Total paths: " <> show (length allPaths),
          "Queued paths: " <> show (length missingPaths)
        ]

newPushJob :: (MonadIO m) => Protocol.PushRequest -> m PushJob
newPushJob pushRequest = do
  pushId <- Protocol.newPushRequestId
  pushCreatedAt <- liftIO getCurrentTime
  let pushDetails = newPushDetails
  return $ PushJob {..}

newPushDetails :: PushDetails
newPushDetails =
  PushDetails
    { pdAllPaths = mempty,
      pdQueuedPaths = mempty,
      pdPushedPaths = mempty,
      pdFailedPaths = mempty,
      pdSkippedPaths = mempty
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
          Katip.logFM Katip.InfoS (Katip.ls $ "Failed " <> (toS sp :: Text))
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

-- Push events

pushStarted :: Protocol.PushRequestId -> PushManager ()
pushStarted pushId = do
  sendPushEvent <- asks pmOnPushEvent
  liftIO $ do
    timestamp <- getCurrentTime
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushStarted timestamp)

pushFinished :: Protocol.PushRequestId -> PushManager ()
pushFinished pushId = do
  sendPushEvent <- asks pmOnPushEvent
  liftIO $ do
    timestamp <- getCurrentTime
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushFinished timestamp)

pushStorePathAttempt :: FilePath -> Int64 -> RetryStatus -> PushManager ()
pushStorePathAttempt storePath size retryStatus = do
  let pushRetryStatus = newPushRetryStatus retryStatus
  timestamp <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  pushIds <- lookupPushIdsForStorePath (toS storePath)
  liftIO $ for_ pushIds $ \pushId ->
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushStorePathAttempt storePath size pushRetryStatus)

pushStorePathProgress :: FilePath -> Int64 -> Int64 -> PushManager ()
pushStorePathProgress storePath currentBytes newBytes = do
  timestamp <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  pushIds <- lookupPushIdsForStorePath (toS storePath)
  liftIO $ for_ pushIds $ \pushId ->
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushStorePathProgress storePath currentBytes newBytes)

pushStorePathDone :: FilePath -> PushManager ()
pushStorePathDone storePath = do
  timestamp <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  pushIds <- lookupPushIdsForStorePath (toS storePath)
  for_ pushIds $ \pushId ->
    liftIO $
      sendPushEvent pushId $
        PushEvent timestamp pushId (PushStorePathDone storePath)

  removeStorePath storePath

pushStorePathFailed :: FilePath -> Text -> PushManager ()
pushStorePathFailed storePath errMsg = do
  timestamp <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  pushIds <- lookupPushIdsForStorePath (toS storePath)
  for_ pushIds $ \pushId ->
    liftIO $
      sendPushEvent pushId $
        PushEvent timestamp pushId (PushStorePathFailed storePath errMsg)

normalizeStorePath :: (MonadIO m) => Store -> FilePath -> m (Maybe StorePath)
normalizeStorePath store fp =
  liftIO $ runMaybeT $ do
    storePath <- MaybeT $ followLinksToStorePath store (encodeUtf8 $ T.pack fp)
    MaybeT $ filterInvalidStorePath store storePath
