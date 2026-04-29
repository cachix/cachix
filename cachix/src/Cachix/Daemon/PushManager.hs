module Cachix.Daemon.PushManager
  ( newPushManagerEnv,
    runPushManager,
    drainPushManager,
    closePushManager,

    -- * Push strategy
    newPushStrategy,

    -- * Push job
    PushJob (..),
    newPushJob,
    addPushJob,
    addPushJobFromRequest,
    lookupPushJob,
    withPushJob,
    resolvePushJob,
    pendingJobCount,

    -- * Query
    filterPushJobs,
    getFailedPushJobs,
    failPendingJobs,

    -- * Store paths
    queueStorePaths,
    removeStorePath,
    queuedStorePathCount,

    -- * Tasks
    handleTask,

    -- * Push events
    pushStarted,
    pushFinished,
    pushStorePathAttempt,
    pushStorePathProgress,
    pushStorePathDone,
    pushStorePathFailed,

    -- * Batch processor
    startBatchProcessor,
    stopBatchProcessor,

    -- * Helpers
    atomicallyWithTimeout,
  )
where

import Cachix.Client.CNix (formatStorePathError, logStorePathWarning, resolveStorePaths)
import Cachix.Client.Command.Push hiding (pushStrategy)
import Cachix.Client.OptionsParser as Client.OptionsParser
  ( PushOptions (..),
  )
import Cachix.Client.Push as Client.Push
import Cachix.Client.Retry (retryAll)
import Cachix.Daemon.NarinfoQuery qualified as NarinfoQuery
import Cachix.Daemon.Protocol qualified as Protocol
import Cachix.Daemon.PushManager.PushJob qualified as PushJob
import Cachix.Daemon.ShutdownLatch qualified as ShutdownLatch
import Cachix.Daemon.TaskQueue
import Cachix.Daemon.Types.Log (Logger)
import Cachix.Daemon.Types.PushEvent (PushEvent (..), PushEventMessage (..), newPushRetryStatus)
import Cachix.Daemon.Types.PushManager
import Cachix.Types.BinaryCache qualified as BinaryCache
import Conduit qualified as C
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM.TVar
import Control.Monad.Catch qualified as E
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Retry (RetryStatus, rsIterNumber)
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.IORef
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (UTCTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import GHC.Clock (getMonotonicTimeNSec)
import Hercules.CNix (StorePath)
import Hercules.CNix.Store (Store, parseStorePath, storePathToPath)
import Katip qualified
import Protolude hiding (toS)
import Protolude.Conv (toS)
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Conduit ()
import UnliftIO.QSem qualified as QSem

newPushManagerEnv :: (MonadIO m) => PushOptions -> NarinfoQuery.NarinfoQueryOptions -> PushParams PushManager () -> OnPushEvent -> Logger -> m PushManagerEnv
newPushManagerEnv pushOptions batchOptions pmPushParams onPushEvent pmLogger = liftIO $ do
  pmPushJobs <- newTVarIO mempty
  pmPendingJobCount <- newTVarIO 0
  pmStorePathIndex <- newTVarIO mempty
  pmTaskQueue <- atomically newTaskQueue
  pmTaskSemaphore <- QSem.newQSem (numJobs pushOptions)
  pmLastEventTimestamp <- newTVarIO =<< getCurrentTime
  let pmProgressEmitIntervalNs = 200 * 1000 * 1000
  let pmOnPushEvent id pushEvent = updateTimestampTVar pmLastEventTimestamp >> onPushEvent id pushEvent

  -- Create query manager with callback that queues ProcessQueryResponse tasks
  let batchCallback requestId response = do
        atomically $ writeTask pmTaskQueue $ HandleMissingPathsResponse requestId response
  pmNarinfoQueryManager <- NarinfoQuery.new batchOptions batchCallback
  pmShutdownLatch <- ShutdownLatch.newShutdownLatch

  return $ PushManagerEnv {..}

runPushManager :: (MonadIO m) => PushManagerEnv -> PushManager a -> m a
runPushManager env f = liftIO $ unPushManager f `runReaderT` env

-- | Set the shutdown latch (rejecting new jobs), then wait for all in-flight jobs to complete with an idle timeout.
--
-- Returns True if all jobs completed, False if no progress was observed within the timeout window.
drainPushManager :: TimeoutOptions -> PushManagerEnv -> IO Bool
drainPushManager timeoutOptions PushManagerEnv {..} = do
  ShutdownLatch.initiateShutdown () pmShutdownLatch
  -- Reset the last event timestamp to start off the countdown from here
  updateTimestampTVar pmLastEventTimestamp
  atomicallyWithTimeout timeoutOptions pmLastEventTimestamp $ do
    pendingJobs <- readTVar pmPendingJobCount
    check (pendingJobs <= 0)

-- | Close the task queue so workers see it as closed and exit.
closePushManager :: PushManagerEnv -> IO ()
closePushManager PushManagerEnv {..} =
  atomically $ closeTaskQueue pmTaskQueue

-- | Start the batch processor for narinfo queries
startBatchProcessor :: (MonadUnliftIO m, Katip.KatipContext m) => PushManagerEnv -> m ()
startBatchProcessor env@PushManagerEnv {pmNarinfoQueryManager} = do
  NarinfoQuery.start pmNarinfoQueryManager $ \paths ->
    runPushManager env (processBatchedNarinfo paths)
  where
    -- Process a batch of store paths for narinfo queries
    processBatchedNarinfo :: [StorePath] -> PushManager ([StorePath], [StorePath])
    processBatchedNarinfo storePaths = do
      pushParams <- asks pmPushParams
      queryNarInfoBulk pushParams storePaths

-- | Stop the batch processor
stopBatchProcessor :: (MonadIO m) => PushManagerEnv -> m ()
stopBatchProcessor PushManagerEnv {pmNarinfoQueryManager} = do
  NarinfoQuery.stop pmNarinfoQueryManager

-- Manage push jobs

newPushJob :: (MonadIO m) => Protocol.PushRequest -> m PushJob
newPushJob = PushJob.new

addPushJob :: PushJob -> PushManager Bool
addPushJob pushJob = do
  PushManagerEnv {..} <- ask
  let pushId = PushJob.pushId pushJob

  Katip.logLocM Katip.DebugS $ Katip.ls $ "Queued push job " <> (show pushId :: Text)

  let queueJob = do
        shuttingDown <- ShutdownLatch.isShuttingDownSTM pmShutdownLatch
        if shuttingDown
          then return False
          else do
            modifyTVar' pmPushJobs $ HashMap.insert pushId pushJob
            incrementTVar pmPendingJobCount
            res <- tryWriteTask pmTaskQueue $ QueryMissingPaths pushId
            case res of
              Just True -> return True
              _ -> return False

  didQueue <- liftIO $ atomically queueJob

  unless didQueue $
    Katip.logLocM Katip.WarningS "Failed to queue push job. Queue likely full."

  return didQueue

addPushJobFromRequest :: Protocol.PushRequest -> PushManager (Maybe Protocol.PushRequestId)
addPushJobFromRequest pushRequest = do
  pushJob <- newPushJob pushRequest
  success <- addPushJob pushJob
  return $ if success then Just (PushJob.pushId pushJob) else Nothing

removePushJob :: Protocol.PushRequestId -> PushManager ()
removePushJob pushId = do
  PushManagerEnv {..} <- ask
  liftIO $ atomically $ do
    mpushJob <- HashMap.lookup pushId <$> readTVar pmPushJobs
    for_ mpushJob $ \pushJob -> do
      -- Decrement the job count if this job had not been processed yet
      unless (PushJob.isProcessed pushJob) (decrementTVar pmPendingJobCount)
      modifyTVar' pmPushJobs (HashMap.delete pushId)

lookupPushJob :: Protocol.PushRequestId -> PushManager (Maybe PushJob)
lookupPushJob pushId = do
  pushJobs <- asks pmPushJobs
  liftIO $ HashMap.lookup pushId <$> readTVarIO pushJobs

filterPushJobs :: (PushJob -> Bool) -> PushManager [PushJob]
filterPushJobs f = do
  pushJobs <- asks pmPushJobs
  liftIO $ filter f . HashMap.elems <$> readTVarIO pushJobs

getFailedPushJobs :: PushManager [PushJob]
getFailedPushJobs = filterPushJobs PushJob.isFailed

withPushJob :: Protocol.PushRequestId -> (PushJob -> PushManager ()) -> PushManager ()
withPushJob pushId f =
  maybe handleMissingPushJob f =<< lookupPushJob pushId
  where
    handleMissingPushJob =
      Katip.logLocM Katip.ErrorS $ Katip.ls $ "Push job " <> (show pushId :: Text) <> " not found"

-- | Apply an update to many push jobs atomically. After the update, a job
-- transitions to a terminal state if either the update made it processed
-- directly or it leaves 'pushQueue' empty. Returns the jobs that transitioned
-- to a terminal state in this call.
--
-- Mark, completion check, and pending-counter decrement happen in one STM
-- transaction so concurrent workers cannot race past the empty-queue check.
applyPushJobUpdates ::
  (Foldable t) =>
  t Protocol.PushRequestId ->
  (UTCTime -> PushJob -> PushJob) ->
  PushManager [PushJob]
applyPushJobUpdates pushIds update = do
  PushManagerEnv {pmPushJobs, pmPendingJobCount} <- ask
  ts <- liftIO getCurrentTime
  liftIO $ atomically $ do
    jobs <- readTVar pmPushJobs
    let (jobs', finished) = foldl' (step ts) (jobs, []) pushIds
    writeTVar pmPushJobs jobs'
    modifyTVar' pmPendingJobCount (subtract (length finished))
    pure finished
  where
    step ts (!jobs, acc) pushId =
      case HashMap.lookup pushId jobs of
        Just job
          | not (PushJob.isProcessed job) ->
              let job' = transitionIfDone ts (update ts job)
                  jobs' = HashMap.insert pushId job' jobs
               in if PushJob.isProcessed job'
                    then (jobs', job' : acc)
                    else (jobs', acc)
        _ -> (jobs, acc)

    transitionIfDone ts job
      | PushJob.isProcessed job = job
      | Set.null (PushJob.pushQueue job) =
          if PushJob.hasFailedPaths job
            then PushJob.fail ts job
            else PushJob.complete ts job
      | otherwise = job

failPushJob :: Protocol.PushRequestId -> PushManager ()
failPushJob pushId = void $ applyPushJobUpdates [pushId] PushJob.fail

-- | Mark every non-terminal job as failed and emit failure events for any
-- paths still in their queues. Failed jobs stay in 'pmPushJobs' so a later
-- 'getFailedPushJobs' call sees them and the daemon exits with the right
-- code; the natural path removes jobs via 'pushFinished'.
failPendingJobs :: Text -> PushManager [PushJob]
failPendingJobs reason = do
  pmPushJobs <- asks pmPushJobs
  allIds <- HashMap.keys <$> liftIO (readTVarIO pmPushJobs)
  failed <- applyPushJobUpdates allIds PushJob.fail

  ts <- liftIO getCurrentTime
  sendPushEvent <- asks pmOnPushEvent
  for_ failed $ \job -> do
    let pid = PushJob.pushId job
    for_ (PushJob.pushQueue job) $ \path ->
      sendStorePathEventAt ts [pid] (PushStorePathFailed path reason)
    liftIO $ sendPushEvent pid (PushEvent ts pid PushFinished)

  pure failed

pendingJobCount :: PushManager Int
pendingJobCount = do
  pmPendingJobCount <- asks pmPendingJobCount
  liftIO $ readTVarIO pmPendingJobCount

-- Manage store paths

queueStorePaths :: Protocol.PushRequestId -> [FilePath] -> PushManager ()
queueStorePaths pushId storePaths = do
  PushManagerEnv {..} <- ask

  let addToQueue storePath = do
        isDuplicate <- HashMap.member storePath <$> readTVar pmStorePathIndex
        unless isDuplicate $
          writeTask pmTaskQueue (PushPath storePath)

        modifyTVar' pmStorePathIndex $ HashMap.insertWith (<>) storePath (Seq.singleton pushId)

  transactionally $ map addToQueue storePaths

removeStorePath :: FilePath -> PushManager ()
removeStorePath storePath = do
  storePathIndex <- asks pmStorePathIndex
  liftIO $ atomically $ do
    modifyTVar' storePathIndex $ HashMap.delete storePath

lookupStorePathIndex :: FilePath -> PushManager (Seq.Seq Protocol.PushRequestId)
lookupStorePathIndex storePath = do
  storePathIndex <- asks pmStorePathIndex
  references <- liftIO $ readTVarIO storePathIndex
  return $ fromMaybe Seq.empty (HashMap.lookup storePath references)

queuedStorePathCount :: PushManager Integer
queuedStorePathCount = do
  pmPushJobs <- asks pmPushJobs
  jobs <- liftIO $ readTVarIO pmPushJobs
  pure $ foldl' countQueuedPaths 0 (HashMap.elems jobs)
  where
    countQueuedPaths acc job = acc + fromIntegral (Set.size $ pushQueue job)

resolvePushJob :: Protocol.PushRequestId -> PushJob.ResolvedClosure FilePath -> PushManager ()
resolvePushJob pushId closure = do
  Katip.logLocM Katip.DebugS $ Katip.ls $ showClosureStats closure

  finishedJobs <- applyPushJobUpdates [pushId] (PushJob.populateQueue closure)

  withPushJob pushId $ \pushJob -> do
    pushStarted pushJob
    let skippedPaths = Set.difference (PushJob.rcAllPaths closure) (PushJob.rcMissingPaths closure)
    ts <- liftIO getCurrentTime
    forM_ skippedPaths $ \path ->
      sendStorePathEventAt ts [pushId] (PushStorePathSkipped path)
    queueStorePaths pushId $ Set.toList (PushJob.rcMissingPaths closure)

  for_ finishedJobs pushFinished
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

handleTask :: Task -> PushManager ()
handleTask task = do
  pushParams <- asks pmPushParams
  case task of
    QueryMissingPaths pushId ->
      runQueryMissingPathsTask pushParams pushId
    HandleMissingPathsResponse pushId batchResponse ->
      runHandleMissingPathsResponseTask pushParams pushId batchResponse
    PushPath filePath ->
      runPushPathTask pushParams filePath

runQueryMissingPathsTask :: PushParams PushManager () -> Protocol.PushRequestId -> PushManager ()
runQueryMissingPathsTask pushParams pushId =
  resolveClosure `withException` failJob
  where
    failJob :: SomeException -> PushManager ()
    failJob err = do
      failPushJob pushId

      Katip.katipAddContext (Katip.sl "error" (displayException err)) $
        Katip.logLocM Katip.ErrorS $
          Katip.ls $
            "Failed to resolve closure for push job " <> (show pushId :: Text)

    resolveClosure = do
      Katip.logLocM Katip.DebugS $ Katip.ls $ "Resolving closure for push job " <> (show pushId :: Text)

      withPushJob pushId $ \pushJob -> do
        let sps = Protocol.storePaths (pushRequest pushJob)
            store = pushParamsStore pushParams

        -- Resolve paths and track which ones are invalid
        (errors, validPaths) <- liftIO $ resolveStorePaths store sps

        -- Log warnings for invalid paths
        liftIO $ for_ errors $ uncurry logStorePathWarning

        -- Emit PushStorePathInvalid events for invalid paths
        ts <- liftIO getCurrentTime
        forM_ errors $ \(path, err) ->
          sendStorePathEventAt ts [pushId] (PushStorePathInvalid path (formatStorePathError err))

        paths <- computeClosure store validPaths

        -- Use async batch manager for narinfo queries (non-blocking)
        batchManager <- asks pmNarinfoQueryManager
        NarinfoQuery.submitRequest batchManager pushId paths

runHandleMissingPathsResponseTask :: PushParams PushManager () -> Protocol.PushRequestId -> NarinfoQuery.NarinfoResponse -> PushManager ()
runHandleMissingPathsResponseTask pushParams pushId batchResponse =
  processQueryResponse `withException` failJob
  where
    failJob :: SomeException -> PushManager ()
    failJob err = do
      failPushJob pushId

      Katip.katipAddContext (Katip.sl "error" (displayException err)) $
        Katip.logLocM Katip.ErrorS $
          Katip.ls $
            "Failed to process batch response for push job " <> (show pushId :: Text)

    processQueryResponse = do
      Katip.logLocM Katip.DebugS $ Katip.ls $ "Processing batch response for push job " <> (show pushId :: Text)

      let allStorePaths = Set.toList $ NarinfoQuery.nrAllPaths batchResponse
          missingStorePaths = Set.toList $ NarinfoQuery.nrMissingPaths batchResponse
          store = pushParamsStore pushParams

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

runPushPathTask :: PushParams PushManager () -> FilePath -> PushManager ()
runPushPathTask pushParams filePath = do
  pushStorePath `withException` failStorePath
  where
    failStorePath =
      pushStorePathFailed filePath . toS . displayException

    pushStorePath = do
      qs <- asks pmTaskSemaphore
      E.bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) $ do
        Katip.logLocM Katip.DebugS $ Katip.ls $ "Pushing store path " <> filePath

        let store = pushParamsStore pushParams
        storePath <- liftIO $ parseStorePath store (toS filePath)

        retryAll $ uploadStorePath pushParams storePath

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
          Katip.logFM Katip.InfoS (Katip.ls $ "Failed to push " <> (toS sp :: Text))
        pushStorePathFailed (toS sp) errText

      onAttempt retryStatus size = do
        sp <- liftIO $ storePathToPath store storePath
        let retryContext =
              if rsIterNumber retryStatus > 0
                then Katip.katipAddContext (Katip.sl "retry" (rsIterNumber retryStatus))
                else identity
        retryContext $
          Katip.logFM Katip.InfoS $
            Katip.ls $
              "Pushing " <> (toS sp :: Text)
        pushStorePathAttempt (toS sp) size retryStatus

      onUncompressedNARStream _ size = do
        sp <- liftIO $ storePathToPath store storePath
        progressEmitIntervalNs <- asks pmProgressEmitIntervalNs
        lastEmitNsRef <- liftIO $ newIORef =<< getMonotonicTimeNSec
        currentBytesRef <- liftIO $ newIORef (0 :: Int64)
        lastEmittedBytesRef <- liftIO $ newIORef (0 :: Int64)
        C.awaitForever $ \chunk -> do
          let newBytes = fromIntegral (BS.length chunk)
          currentBytes <- liftIO $ atomicModifyIORef' currentBytesRef (\b -> (b + newBytes, b + newBytes))
          lastEmitNs <- liftIO $ readIORef lastEmitNsRef
          nowNs <- liftIO getMonotonicTimeNSec

          when (nowNs - lastEmitNs >= progressEmitIntervalNs || currentBytes == size) $ do
            liftIO $ writeIORef lastEmitNsRef nowNs
            lastEmitted <- liftIO $ readIORef lastEmittedBytesRef
            let emitBytes = currentBytes - lastEmitted
            liftIO $ writeIORef lastEmittedBytesRef currentBytes
            when (emitBytes > 0) $
              lift $
                lift $
                  pushStorePathProgress (toS sp) currentBytes emitBytes

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
          Client.Push.chunkSize = Client.OptionsParser.chunkSize opts,
          Client.Push.numConcurrentChunks = Client.OptionsParser.numConcurrentChunks opts,
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
  let defaultDuration = 0
  pushDuration <- MaybeT $ pure $ Just (fromMaybe defaultDuration $ PushJob.duration pushJob)

  defaultCompletedAt <- liftIO getCurrentTime
  completedAt <- MaybeT $ pure $ Just (fromMaybe defaultCompletedAt $ PushJob.completedAt pushJob)

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

sendStorePathEvent :: (Foldable f) => f Protocol.PushRequestId -> PushEventMessage -> PushManager ()
sendStorePathEvent pushIds msg = do
  timestamp <- liftIO getCurrentTime
  sendStorePathEventAt timestamp pushIds msg

sendStorePathEventAt :: (Foldable f) => UTCTime -> f Protocol.PushRequestId -> PushEventMessage -> PushManager ()
sendStorePathEventAt timestamp pushIds msg = do
  sendPushEvent <- asks pmOnPushEvent
  liftIO $ forM_ pushIds $ \pushId ->
    sendPushEvent pushId (PushEvent timestamp pushId msg)

pushStorePathAttempt :: FilePath -> Int64 -> RetryStatus -> PushManager ()
pushStorePathAttempt storePath size retryStatus = do
  let pushRetryStatus = newPushRetryStatus retryStatus
  pushIds <- lookupStorePathIndex storePath
  sendStorePathEvent pushIds (PushStorePathAttempt storePath size pushRetryStatus)

pushStorePathProgress :: FilePath -> Int64 -> Int64 -> PushManager ()
pushStorePathProgress storePath currentBytes newBytes = do
  pushIds <- lookupStorePathIndex storePath
  sendStorePathEvent pushIds (PushStorePathProgress storePath currentBytes newBytes)

pushStorePathDone :: FilePath -> PushManager ()
pushStorePathDone storePath = do
  pushIds <- lookupStorePathIndex storePath
  finishedJobs <- applyPushJobUpdates pushIds (\_ -> PushJob.markStorePathPushed storePath)
  sendStorePathEvent pushIds (PushStorePathDone storePath)
  for_ finishedJobs pushFinished
  removeStorePath storePath

pushStorePathFailed :: FilePath -> Text -> PushManager ()
pushStorePathFailed storePath errMsg = do
  pushIds <- lookupStorePathIndex storePath
  finishedJobs <- applyPushJobUpdates pushIds (\_ -> PushJob.markStorePathFailed storePath)
  sendStorePathEvent pushIds (PushStorePathFailed storePath errMsg)
  for_ finishedJobs pushFinished
  removeStorePath storePath

-- Helpers

storeToFilePath :: (MonadIO m) => Store -> StorePath -> m FilePath
storeToFilePath store storePath = do
  fp <- liftIO $ storePathToPath store storePath
  pure $ toS fp

withException :: (E.MonadCatch m) => m a -> (SomeException -> m a) -> m a
withException action handler = action `E.catchAll` (\e -> handler e >> E.throwM e)

-- STM helpers

transactionally :: (Foldable t, MonadIO m) => t (STM ()) -> m ()
transactionally = liftIO . atomically . sequence_

updateTimestampTVar :: (MonadIO m) => TVar UTCTime -> m ()
updateTimestampTVar tvar = liftIO $ do
  now <- getCurrentTime
  atomically $ writeTVar tvar now

incrementTVar :: TVar Int -> STM ()
incrementTVar tvar = modifyTVar' tvar (+ 1)

decrementTVar :: TVar Int -> STM ()
decrementTVar tvar = modifyTVar' tvar (subtract 1)

-- | Run a transaction with a timeout.
-- Returns True if the transaction completed, False if timed out.
atomicallyWithTimeout ::
  TimeoutOptions ->
  -- | A TVar timestamp to compare against
  TVar UTCTime ->
  -- | The transaction to run
  STM () ->
  IO Bool
atomicallyWithTimeout TimeoutOptions {..} timeVar transaction = do
  timeoutVar <- newTVarIO False
  Async.withAsync (updateShutdownTimeout timeoutVar) $ \_ ->
    waitForGracefulShutdown timeoutVar
  where
    waitForGracefulShutdown timeout =
      atomically $
        (transaction >> return True)
          `orElse` (checkShutdownTimeout timeout >> return False)

    updateShutdownTimeout timeoutVar =
      forever $ do
        now <- getCurrentTime
        atomically $ do
          timestamp <- readTVar timeVar
          let isTimeout =
                secondsToNominalDiffTime (realToFrac toTimeout) <= now `diffUTCTime` timestamp
          writeTVar timeoutVar isTimeout
        threadDelay $ ceiling (toPollingInterval * 1000.0 * 1000.0)

    checkShutdownTimeout timeout = check =<< readTVar timeout
