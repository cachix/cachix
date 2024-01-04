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
    withPushJob,
    resolvePushJob,

    -- * Store paths
    queueStorePaths,
    removeStorePath,
    queuedStorePathCount,

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
import qualified Cachix.Client.Daemon.PushManager.PushJob as PushJob
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
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Hercules.CNix (StorePath)
import Hercules.CNix.Store (Store, parseStorePath, storePathToPath)
import qualified Katip
import Protolude hiding (toS)
import Protolude.Conv (toS)
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Conduit ()
import qualified UnliftIO.QSem as QSem

newPushManagerEnv :: (MonadIO m) => PushOptions -> Logger -> OnPushEvent -> m PushManagerEnv
newPushManagerEnv pushOptions pmLogger pmOnPushEvent = liftIO $ do
  pmPushJobs <- newTVarIO mempty
  pmStorePathReferences <- newTVarIO mempty
  pmTaskQueue <- newTBMQueueIO 1000
  pmTaskSemaphore <- QSem.newQSem (numJobs pushOptions)
  return $ PushManagerEnv {..}

runPushManager :: (MonadIO m) => PushManagerEnv -> PushManager a -> m a
runPushManager env f = liftIO $ unPushManager f `runReaderT` env

stopPushManager :: PushManagerEnv -> IO ()
stopPushManager PushManagerEnv {pmTaskQueue, pmPushJobs} =
  atomically $ do
    pushJobs <- readTVar pmPushJobs
    if all PushJob.isCompleted (HashMap.elems pushJobs)
      then closeTBMQueue pmTaskQueue
      else retry

-- Manage push jobs

addPushJob :: Protocol.PushRequest -> PushManager Protocol.PushRequestId
addPushJob pushRequest = do
  PushManagerEnv {..} <- ask
  pushJob <- PushJob.new pushRequest

  Katip.logLocM Katip.DebugS $ Katip.ls $ "Queued push job " <> (show (pushId pushJob) :: Text)

  liftIO $
    atomically $ do
      modifyTVar' pmPushJobs $ HashMap.insert (pushId pushJob) pushJob
      writeTBMQueue pmTaskQueue $ ResolveClosure (pushId pushJob)

  return (pushId pushJob)

removePushJob :: Protocol.PushRequestId -> PushManager ()
removePushJob pushId = do
  PushManagerEnv {..} <- ask
  liftIO $ atomically $ modifyTVar' pmPushJobs $ HashMap.delete pushId

lookupPushJob :: Protocol.PushRequestId -> PushManager (Maybe PushJob)
lookupPushJob pushId = do
  pushJobs <- asks pmPushJobs
  liftIO $ HashMap.lookup pushId <$> readTVarIO pushJobs

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
    let pj = HashMap.adjust f pushId jobs
    (HashMap.lookup pushId pj, pj)

modifyPushJobs :: [Protocol.PushRequestId] -> (PushJob -> PushJob) -> PushManager ()
modifyPushJobs pushIds f = do
  pushJobs <- asks pmPushJobs
  liftIO $ atomically $ modifyTVar' pushJobs $ \pushJobs' ->
    foldl' (flip (HashMap.adjust f)) pushJobs' pushIds

-- Manage store paths

queueStorePaths :: Protocol.PushRequestId -> [FilePath] -> PushManager ()
queueStorePaths pushId storePaths = do
  PushManagerEnv {..} <- ask

  let addToQueue storePath = do
        isDuplicate <- HashMap.member storePath <$> readTVar pmStorePathReferences
        unless isDuplicate $
          writeTBMQueue pmTaskQueue (PushStorePath storePath)

        modifyTVar' pmStorePathReferences $ HashMap.insertWith (<>) storePath [pushId]

  transactionally $ map addToQueue storePaths

removeStorePath :: FilePath -> PushManager ()
removeStorePath storePath = do
  pmStorePathReferences <- asks pmStorePathReferences
  liftIO $ atomically $ do
    modifyTVar' pmStorePathReferences $ HashMap.delete storePath

lookupStorePathReferences :: FilePath -> PushManager [Protocol.PushRequestId]
lookupStorePathReferences storePath = do
  pmStorePathReferences <- asks pmStorePathReferences
  references <- liftIO $ readTVarIO pmStorePathReferences
  return $ fromMaybe [] (HashMap.lookup storePath references)

checkPushJobCompleted :: Protocol.PushRequestId -> PushManager ()
checkPushJobCompleted pushId = do
  mpushJob <- lookupPushJob pushId
  for_ mpushJob $ \pushJob ->
    when (Set.null $ pushQueue pushJob) $ do
      timestamp <- liftIO getCurrentTime
      _ <- modifyPushJob pushId $ PushJob.complete timestamp
      pushFinished pushJob

queuedStorePathCount :: PushManager Integer
queuedStorePathCount = do
  pmPushJobs <- asks pmPushJobs
  jobs <- liftIO $ readTVarIO pmPushJobs
  pure $ foldl' countQueuedPaths 0 (HashMap.elems jobs)
  where
    countQueuedPaths acc job = acc + fromIntegral (Set.size $ pushQueue job)

resolvePushJob :: Protocol.PushRequestId -> PushJob.ResolvedClosure FilePath -> PushManager ()
resolvePushJob pushId closure = do
  timestamp <- liftIO getCurrentTime

  _ <- modifyPushJob pushId $ PushJob.populateQueue closure timestamp

  withPushJob pushId $ \pushJob -> do
    Katip.logLocM Katip.DebugS $ Katip.ls $ showClosureStats closure

    pushStarted pushJob
    -- Create STM action for each path and then run everything atomically
    queueStorePaths pushId $ Set.toList (PushJob.rcMissingPaths closure)
    -- Check if the job is already completed, i.e. all paths have been skipped.
    checkPushJobCompleted pushId
  where
    showClosureStats :: PushJob.ResolvedClosure FilePath -> Text
    showClosureStats PushJob.ResolvedClosure {..} =
      let skippedPaths = Set.difference rcAllPaths rcMissingPaths
          queuedCount = length rcMissingPaths
          skippedCount = length skippedPaths
          totalCount = queuedCount + skippedCount
       in T.intercalate
            "\n"
            [ "Resolved push job " <> show pushId,
              "Total paths: " <> show totalCount,
              "Queued paths: " <> show queuedCount,
              "Skipped paths: " <> show skippedCount
            ]

handleTask :: PushParams PushManager () -> Task -> PushManager ()
handleTask pushParams task = do
  case task of
    ResolveClosure pushId -> do
      Katip.logLocM Katip.DebugS $ Katip.ls $ "Resolving closure for push job " <> (show pushId :: Text)

      withPushJob pushId $ \pushJob -> do
        let sps = Protocol.storePaths (pushRequest pushJob)
            store = pushParamsStore pushParams
        normalized <- mapM (normalizeStorePath store) sps
        (allStorePaths, missingStorePaths) <- getMissingPathsForClosure pushParams (catMaybes normalized)
        storePathsToPush <- pushOnClosureAttempt pushParams allStorePaths missingStorePaths

        resolvedClosure <- do
          allPaths <- liftIO $ mapM (storeToFilePath store) allStorePaths
          pathsToPush <- liftIO $ mapM (storeToFilePath store) storePathsToPush
          return $
            PushJob.ResolvedClosure
              { rcAllPaths = Set.fromList allPaths,
                rcMissingPaths = Set.fromList pathsToPush
              }

        resolvePushJob pushId resolvedClosure
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
pushStarted pushJob@PushJob {pushId} = do
  case PushJob.startedAt pushJob of
    Nothing -> return ()
    Just timestamp -> do
      sendPushEvent <- asks pmOnPushEvent
      liftIO $ do
        sendPushEvent pushId $
          PushEvent timestamp pushId PushStarted

pushFinished :: PushJob -> PushManager ()
pushFinished pushJob@PushJob {pushId} = void $ runMaybeT $ do
  pushDuration <- MaybeT $ pure $ PushJob.duration pushJob
  completedAt <- MaybeT $ pure $ PushJob.completedAt pushJob

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
      PushEvent completedAt pushId PushFinished

  lift $ removePushJob pushId

sendStorePathEvent :: [Protocol.PushRequestId] -> PushEventMessage -> PushManager ()
sendStorePathEvent pushIds msg = do
  timestamp <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  liftIO $ forM_ pushIds $ \pushId ->
    sendPushEvent pushId (PushEvent timestamp pushId msg)

pushStorePathAttempt :: FilePath -> Int64 -> RetryStatus -> PushManager ()
pushStorePathAttempt storePath size retryStatus = do
  let pushRetryStatus = newPushRetryStatus retryStatus
  pushIds <- lookupStorePathReferences storePath
  sendStorePathEvent pushIds (PushStorePathAttempt storePath size pushRetryStatus)

pushStorePathProgress :: FilePath -> Int64 -> Int64 -> PushManager ()
pushStorePathProgress storePath currentBytes newBytes = do
  pushIds <- lookupStorePathReferences storePath
  sendStorePathEvent pushIds (PushStorePathProgress storePath currentBytes newBytes)

pushStorePathDone :: FilePath -> PushManager ()
pushStorePathDone storePath = do
  pushIds <- lookupStorePathReferences storePath
  modifyPushJobs pushIds (PushJob.markStorePathPushed storePath)

  sendStorePathEvent pushIds (PushStorePathDone storePath)

  forM_ pushIds checkPushJobCompleted

  removeStorePath storePath

pushStorePathFailed :: FilePath -> Text -> PushManager ()
pushStorePathFailed storePath errMsg = do
  pushIds <- lookupStorePathReferences storePath
  modifyPushJobs pushIds (PushJob.markStorePathFailed storePath)

  sendStorePathEvent pushIds (PushStorePathFailed storePath errMsg)

  forM_ pushIds checkPushJobCompleted

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
