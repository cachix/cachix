module Cachix.Client.Daemon.PushManager
  ( newPushManagerEnv,
    runPushManager,
    stopPushManager,

    -- * Push strategy
    newPushStrategy,

    -- * Push job
    PushJob (..),
    addPushJob,
    lookupPushJob,
    resolvePushJob,

    -- * Store paths
    queueStorePath,
    removeStorePath,

    -- * Tasks
    handleTask,
    -- Push events
    pushStarted,
    pushFinished,
    pushStorePathAttempt,
    pushStorePathProgress,
    pushStorePathDone,
    pushStorePathFailed,
  )
where

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
import qualified Control.Exception.Safe as Safe
import qualified Control.Monad.Catch as E
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Retry (RetryStatus)
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import Hercules.CNix (StorePath)
import Hercules.CNix.Store (Store, parseStorePath, storePathToPath)
import qualified Katip
import Protolude hiding (toS)
import Protolude.Conv (toS)
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Conduit ()
import qualified UnliftIO.QSem as QSem

newPushManagerEnv :: (MonadIO m) => Logger -> OnPushEvent -> m PushManagerEnv
newPushManagerEnv pmLogger pmOnPushEvent = liftIO $ do
  pmPushJobs <- newTVarIO mempty
  pmStorePathReferences <- newTVarIO mempty
  pmActiveStorePaths <- newTVarIO mempty
  pmTaskQueue <- newTBMQueueIO 1000
  pmTaskSemaphore <- QSem.newQSem 8
  return $ PushManagerEnv {..}

runPushManager :: (MonadIO m) => PushManagerEnv -> PushManager a -> m a
runPushManager env f = liftIO $ unPushManager f `runReaderT` env

stopPushManager :: PushManagerEnv -> IO ()
stopPushManager PushManagerEnv {pmTaskQueue, pmPushJobs} =
  atomically $ do
    pushJobs <- readTVar pmPushJobs
    let allFinished = all (isJust . pushFinishedAt) $ Map.elems pushJobs
    if allFinished
      then closeTBMQueue pmTaskQueue
      else retry

-- Manage push jobs

addPushJob :: Protocol.PushRequest -> PushManager Protocol.PushRequestId
addPushJob pushRequest = do
  PushManagerEnv {..} <- ask
  pushJob <- newPushJob pushRequest

  Katip.logLocM Katip.DebugS $ Katip.ls $ "Queued push job " <> (show (pushId pushJob) :: Text)

  liftIO $
    atomically $ do
      modifyTVar' pmPushJobs $ Map.insert (pushId pushJob) pushJob
      writeTBMQueue pmTaskQueue $ ResolveClosure (pushId pushJob)

  return (pushId pushJob)

lookupPushJob :: Protocol.PushRequestId -> PushManager (Maybe PushJob)
lookupPushJob pushId = do
  pushJobs <- asks pmPushJobs
  liftIO $ Map.lookup pushId <$> readTVarIO pushJobs

modifyPushJob :: Protocol.PushRequestId -> (PushJob -> PushJob) -> PushManager ()
modifyPushJob pushId f = do
  pushJobs <- asks pmPushJobs
  liftIO $ atomically $ modifyTVar' pushJobs $ Map.adjust f pushId

modifyPushJobs :: [Protocol.PushRequestId] -> (PushJob -> PushJob) -> PushManager ()
modifyPushJobs pushIds f = do
  pushJobs <- asks pmPushJobs
  liftIO $ atomically $ modifyTVar' pushJobs $ \pushJobs' ->
    foldl' (flip (Map.adjust f)) pushJobs' pushIds

newPushJob :: (MonadIO m) => Protocol.PushRequest -> m PushJob
newPushJob pushRequest = do
  pushId <- Protocol.newPushRequestId
  pushCreatedAt <- liftIO getCurrentTime
  let pushStartedAt = Nothing
      pushFinishedAt = Nothing
      pushDetails = newPushDetails
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

-- Manage store paths

queueStorePath :: FilePath -> PushManager ()
queueStorePath storePath = do
  PushManagerEnv {..} <- ask

  mpath <- liftIO $ atomically $ do
    isDuplicate <- Set.member storePath <$> readTVar pmActiveStorePaths

    if isDuplicate
      then pure Nothing
      else do
        writeTBMQueue pmTaskQueue (PushStorePath storePath)
        modifyTVar' pmActiveStorePaths (Set.insert storePath)
        pure $ Just storePath

  case mpath of
    Just _ ->
      Katip.logLocM Katip.DebugS $ Katip.ls $ "Queued store path " <> storePath
    Nothing ->
      Katip.logLocM Katip.DebugS $ Katip.ls $ "Deduped store path " <> storePath

removeStorePath :: FilePath -> PushManager ()
removeStorePath storePath = do
  pmActiveStorePaths <- asks pmActiveStorePaths
  pmStorePathReferences <- asks pmStorePathReferences
  liftIO $ atomically $ do
    modifyTVar' pmActiveStorePaths $ Set.delete storePath
    modifyTVar' pmStorePathReferences $ Map.delete storePath

lookupPushIdsForStorePath :: FilePath -> PushManager [Protocol.PushRequestId]
lookupPushIdsForStorePath storePath = do
  pmStorePathReferences <- asks pmStorePathReferences
  pushIdStore <- liftIO $ readTVarIO pmStorePathReferences
  pure $ fromMaybe [] $ Map.lookup storePath pushIdStore

trackPushIdsForStorePath :: FilePath -> [Protocol.PushRequestId] -> PushManager ()
trackPushIdsForStorePath storePath pushIds = do
  pmStorePathReferences <- asks pmStorePathReferences
  liftIO $ atomically $ modifyTVar' pmStorePathReferences $ Map.insertWith (<>) storePath pushIds

resolvePushJob :: Protocol.PushRequestId -> [FilePath] -> [FilePath] -> PushManager ()
resolvePushJob pushId allPaths missingPaths = do
  let allPathsSet = Set.fromList allPaths
      queuedPathsSet = Set.fromList missingPaths
      skippedPathsSet = Set.difference allPathsSet queuedPathsSet

  modifyPushJob pushId $ \pushJob' -> do
    pushJob'
      { pushDetails =
          (pushDetails pushJob')
            { pdAllPaths = allPathsSet,
              pdQueuedPaths = queuedPathsSet,
              pdSkippedPaths = skippedPathsSet
            }
      }

  pushStarted pushId

  forM_ missingPaths $ \path -> do
    trackPushIdsForStorePath path [pushId]
    queueStorePath path

  lookupPushJob pushId >>= \case
    Nothing -> return ()
    Just (PushJob {pushDetails}) ->
      Katip.logLocM Katip.DebugS $ Katip.ls $ showResolveStats pushDetails

  -- Check if the job is already completed, i.e. all paths have been skipped.
  finishPushJob pushId
  where
    showResolveStats :: PushDetails -> Text
    showResolveStats PushDetails {..} =
      T.intercalate
        "\n"
        [ "Resolved push job " <> show pushId,
          "Total paths: " <> show (length pdAllPaths),
          "Skipped paths: " <> show (length pdSkippedPaths),
          "Queued paths: " <> show (length pdQueuedPaths)
        ]

handleTask :: PushParams PushManager () -> Task -> PushManager ()
handleTask pushParams task = do
  case task of
    ResolveClosure pushId -> do
      Katip.logLocM Katip.InfoS $ Katip.ls $ "Resolving closure for push job " <> (show pushId :: Text)

      let store = pushParamsStore pushParams
      mpushJob <- lookupPushJob pushId

      case mpushJob of
        Nothing ->
          Katip.logLocM Katip.WarningS $ Katip.ls $ "Push job " <> (show pushId :: Text) <> " not found"
        Just pushJob -> do
          let sps = Protocol.storePaths (pushRequest pushJob)
          normalized <- mapM (normalizeStorePath store) sps
          (allStorePaths, missingStorePaths) <- getMissingPathsForClosure pushParams (catMaybes normalized)
          storePaths <- pushOnClosureAttempt pushParams allStorePaths missingStorePaths

          allPaths <- liftIO $ mapM (storeToFilePath store) allStorePaths
          missingPaths <- liftIO $ mapM (storeToFilePath store) storePaths
          resolvePushJob pushId allPaths missingPaths
    PushStorePath filePath -> do
      Katip.logLocM Katip.DebugS $ Katip.ls $ "Pushing store path " <> filePath

      let store = pushParamsStore pushParams
      storePath <- liftIO $ parseStorePath store (toS filePath)

      qs <- asks pmTaskSemaphore
      E.bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) $
        retryAll (uploadStorePath pushParams storePath)
          `Safe.catchAny` (pushStorePathFailed filePath . toS . displayException)

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
  timestamp <- liftIO getCurrentTime

  modifyPushJob pushId $ \pushJob ->
    pushJob {pushStartedAt = Just timestamp}

  sendPushEvent <- asks pmOnPushEvent
  liftIO $ do
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushStarted timestamp)

pushFinished :: Protocol.PushRequestId -> PushManager ()
pushFinished pushId = do
  timestamp <- liftIO getCurrentTime

  modifyPushJob pushId $ \pushJob ->
    pushJob {pushFinishedAt = Just timestamp}

  mpushJob <- lookupPushJob pushId
  case mpushJob of
    Nothing ->
      Katip.logLocM Katip.ErrorS $ Katip.ls $ "Push job " <> (show pushId :: Text) <> " not found"
    Just (PushJob {pushStartedAt, pushFinishedAt}) -> do
      let mpushDuration = do
            startedAt <- pushStartedAt
            finishedAt <- pushFinishedAt
            pure $ diffUTCTime finishedAt startedAt

      for_ mpushDuration $ \pushDuration ->
        Katip.logLocM Katip.InfoS $
          Katip.ls $
            T.intercalate
              " "
              [ "Push job",
                show pushId :: Text,
                "finished in",
                show pushDuration
              ]

  sendPushEvent <- asks pmOnPushEvent
  liftIO $ do
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushFinished timestamp)

pushStorePathAttempt :: FilePath -> Int64 -> RetryStatus -> PushManager ()
pushStorePathAttempt storePath size retryStatus = do
  let pushRetryStatus = newPushRetryStatus retryStatus
  timestamp <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  pushIds <- lookupPushIdsForStorePath storePath
  liftIO $ forM_ pushIds $ \pushId ->
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushStorePathAttempt storePath size pushRetryStatus)

pushStorePathProgress :: FilePath -> Int64 -> Int64 -> PushManager ()
pushStorePathProgress storePath currentBytes newBytes = do
  timestamp <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  pushIds <- lookupPushIdsForStorePath storePath
  liftIO $ forM_ pushIds $ \pushId ->
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushStorePathProgress storePath currentBytes newBytes)

pushStorePathDone :: FilePath -> PushManager ()
pushStorePathDone storePath = do
  timestamp <- liftIO getCurrentTime
  pushIds <- lookupPushIdsForStorePath storePath
  modifyPushJobs pushIds $ \pushJob -> do
    let pd = pushDetails pushJob
        newPushedPaths = Set.insert storePath (pdPushedPaths pd)
        newQueuedPaths = Set.delete storePath (pdQueuedPaths pd)
        pd' = pd {pdPushedPaths = newPushedPaths, pdQueuedPaths = newQueuedPaths}
    pushJob {pushDetails = pd'}

  sendPushEvent <- asks pmOnPushEvent
  liftIO $ forM_ pushIds $ \pushId ->
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushStorePathDone storePath)

  forM_ pushIds finishPushJob

  removeStorePath storePath

pushStorePathFailed :: FilePath -> Text -> PushManager ()
pushStorePathFailed storePath errMsg = do
  timestamp <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  pushIds <- lookupPushIdsForStorePath storePath
  liftIO $ forM_ pushIds $ \pushId ->
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushStorePathFailed storePath errMsg)

  modifyPushJobs pushIds $ \pushJob -> do
    let pd = pushDetails pushJob
        newFailedPaths = Set.insert storePath (pdFailedPaths pd)
        newQueuedPaths = Set.delete storePath (pdQueuedPaths pd)
        pd' = pd {pdFailedPaths = newFailedPaths, pdQueuedPaths = newQueuedPaths}
    pushJob {pushDetails = pd'}

  forM_ pushIds finishPushJob

  removeStorePath storePath

finishPushJob :: Protocol.PushRequestId -> PushManager ()
finishPushJob pushId = do
  mpushJob <- lookupPushJob pushId
  for_ mpushJob $ \PushJob {pushDetails} -> do
    when (Set.size (pdQueuedPaths pushDetails) == 0) $
      pushFinished pushId

-- Helpers

storeToFilePath :: (MonadIO m) => Store -> StorePath -> m FilePath
storeToFilePath store storePath = do
  fp <- liftIO $ storePathToPath store storePath
  pure $ toS fp

normalizeStorePath :: (MonadIO m) => Store -> FilePath -> m (Maybe StorePath)
normalizeStorePath store fp =
  liftIO $ runMaybeT $ do
    storePath <- MaybeT $ followLinksToStorePath store (encodeUtf8 $ T.pack fp)
    MaybeT $ filterInvalidStorePath store storePath
