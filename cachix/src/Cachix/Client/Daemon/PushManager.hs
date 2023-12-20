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

withPushJob :: Protocol.PushRequestId -> (PushJob -> PushManager ()) -> PushManager ()
withPushJob pushId f =
  maybe handleMissingPushJob f =<< lookupPushJob pushId
  where
    handleMissingPushJob =
      Katip.logLocM Katip.ErrorS $ Katip.ls $ "Push job " <> (show pushId :: Text) <> " not found"

modifyPushJob :: Protocol.PushRequestId -> (PushJob -> PushJob) -> PushManager (Maybe PushJob)
modifyPushJob pushId f = do
  pushJobs <- asks pmPushJobs
  liftIO $ atomically $ stateTVar pushJobs $ \jobs -> do
    let pj = Map.adjust f pushId jobs
    (Map.lookup pushId pj, pj)

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

isPushJobFinished :: PushJob -> Bool
isPushJobFinished PushJob {pushDetails} = Set.null (pdQueuedPaths pushDetails)

finishPushJob :: Protocol.PushRequestId -> PushManager ()
finishPushJob pushId = do
  withPushJob pushId $ \pushJob -> do
    when (isPushJobFinished pushJob) $ do
      timestamp <- liftIO getCurrentTime

      modifyPushJob pushId $ \pushJob ->
        pushJob {pushFinishedAt = Just timestamp}

      pushFinished pushJob

-- Manage store paths

queueStorePath :: Protocol.PushRequestId -> FilePath -> PushManager (STM ())
queueStorePath pushId storePath = do
  PushManagerEnv {..} <- ask

  return $ do
    modifyTVar' pmStorePathReferences $ Map.insertWith (<>) storePath [pushId]

    isDuplicate <- Set.member storePath <$> readTVar pmActiveStorePaths
    unless isDuplicate $ do
      writeTBMQueue pmTaskQueue (PushStorePath storePath)
      modifyTVar' pmActiveStorePaths (Set.insert storePath)

removeStorePath :: FilePath -> PushManager ()
removeStorePath storePath = do
  pmActiveStorePaths <- asks pmActiveStorePaths
  pmStorePathReferences <- asks pmStorePathReferences
  liftIO $ atomically $ do
    modifyTVar' pmActiveStorePaths $ Set.delete storePath
    modifyTVar' pmStorePathReferences $ Map.delete storePath

lookupStorePathReferences :: FilePath -> PushManager [Protocol.PushRequestId]
lookupStorePathReferences storePath = do
  pmStorePathReferences <- asks pmStorePathReferences
  pushIdStore <- liftIO $ readTVarIO pmStorePathReferences
  pure $ fromMaybe [] $ Map.lookup storePath pushIdStore

resolvePushJob :: Protocol.PushRequestId -> [FilePath] -> [FilePath] -> PushManager ()
resolvePushJob pushId allPaths missingPaths = do
  let allPathsSet = Set.fromList allPaths
      queuedPathsSet = Set.fromList missingPaths
      skippedPathsSet = Set.difference allPathsSet queuedPathsSet

  timestamp <- liftIO getCurrentTime
  _ <- modifyPushJob pushId $ \pushJob' -> do
    pushJob'
      { pushStartedAt = Just timestamp,
        pushDetails =
          (pushDetails pushJob')
            { pdAllPaths = allPathsSet,
              pdQueuedPaths = queuedPathsSet,
              pdSkippedPaths = skippedPathsSet
            }
      }

  withPushJob pushId $ \pushJob -> do
    Katip.logLocM Katip.DebugS $ Katip.ls $ showResolveStats (pushDetails pushJob)

    pushStarted pushJob

    -- Create STM action for each path and then run everything atomically
    transactionally =<< mapM (queueStorePath pushId) missingPaths

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

      withPushJob pushId $ \pushJob -> do
        let sps = Protocol.storePaths (pushRequest pushJob)
            store = pushParamsStore pushParams
        normalized <- mapM (normalizeStorePath store) sps
        (allStorePaths, missingStorePaths) <- getMissingPathsForClosure pushParams (catMaybes normalized)
        storePathsToPush <- pushOnClosureAttempt pushParams allStorePaths missingStorePaths

        allPaths <- liftIO $ mapM (storeToFilePath store) allStorePaths
        pathsToPush <- liftIO $ mapM (storeToFilePath store) storePathsToPush
        resolvePushJob pushId allPaths pathsToPush
    PushStorePath filePath -> do
      qs <- asks pmTaskSemaphore
      E.bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) $ do
        Katip.logLocM Katip.DebugS $ Katip.ls $ "Pushing store path " <> filePath

        let store = pushParamsStore pushParams
        storePath <- liftIO $ parseStorePath store (toS filePath)

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

pushStarted :: PushJob -> PushManager ()
pushStarted PushJob {pushId, pushStartedAt} = do
  case pushStartedAt of
    Nothing -> return ()
    Just timestamp -> do
      sendPushEvent <- asks pmOnPushEvent
      liftIO $ do
        sendPushEvent pushId $
          PushEvent timestamp pushId (PushStarted timestamp)

pushFinished :: PushJob -> PushManager ()
pushFinished PushJob {pushId, pushStartedAt, pushFinishedAt} = do
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

  case pushFinishedAt of
    Nothing -> return ()
    Just timestamp -> do
      sendPushEvent <- asks pmOnPushEvent
      liftIO $ do
        sendPushEvent pushId $
          PushEvent timestamp pushId (PushFinished timestamp)

pushStorePathAttempt :: FilePath -> Int64 -> RetryStatus -> PushManager ()
pushStorePathAttempt storePath size retryStatus = do
  let pushRetryStatus = newPushRetryStatus retryStatus
  timestamp <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  pushIds <- lookupStorePathReferences storePath
  liftIO $ forM_ pushIds $ \pushId ->
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushStorePathAttempt storePath size pushRetryStatus)

pushStorePathProgress :: FilePath -> Int64 -> Int64 -> PushManager ()
pushStorePathProgress storePath currentBytes newBytes = do
  timestamp <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  pushIds <- lookupStorePathReferences storePath
  liftIO $ forM_ pushIds $ \pushId ->
    sendPushEvent pushId $
      PushEvent timestamp pushId (PushStorePathProgress storePath currentBytes newBytes)

pushStorePathDone :: FilePath -> PushManager ()
pushStorePathDone storePath = do
  timestamp <- liftIO getCurrentTime
  pushIds <- lookupStorePathReferences storePath
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
  pushIds <- lookupStorePathReferences storePath
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

-- Helpers

transactionally :: (Foldable t, MonadIO m) => t (STM ()) -> m ()
transactionally = liftIO . atomically . sequence_

storeToFilePath :: (MonadIO m) => Store -> StorePath -> m FilePath
storeToFilePath store storePath = do
  fp <- liftIO $ storePathToPath store storePath
  pure $ toS fp

normalizeStorePath :: (MonadIO m) => Store -> FilePath -> m (Maybe StorePath)
normalizeStorePath store fp =
  liftIO $ runMaybeT $ do
    storePath <- MaybeT $ followLinksToStorePath store (encodeUtf8 $ T.pack fp)
    MaybeT $ filterInvalidStorePath store storePath
