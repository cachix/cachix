{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cachix.Daemon.NarinfoBatch
  ( -- * Types
    NarinfoBatchManager,
    BatchRequest (..),
    BatchResponse (..),
    NarinfoBatchOptions (..),
    defaultNarinfoBatchOptions,

    -- * Operations
    newNarinfoBatchManager,
    submitBatchRequest,
    startBatchProcessor,
    stopBatchProcessor,
  )
where

import Control.Concurrent.STM
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Hercules.CNix.Store (StorePath)
import Katip qualified
import Protolude
import UnliftIO.Async qualified as Async

-- | Configuration for the narinfo batch manager
data NarinfoBatchOptions = NarinfoBatchOptions
  { -- | Maximum number of paths to accumulate before triggering a batch
    nboMaxBatchSize :: !Int,
    -- | Maximum time to wait before triggering a batch (in seconds)
    -- Use 0 for immediate processing (no batching)
    nboMaxWaitTime :: !NominalDiffTime
  }
  deriving stock (Eq, Show)

-- | Default configuration with reasonable values
defaultNarinfoBatchOptions :: NarinfoBatchOptions
defaultNarinfoBatchOptions =
  NarinfoBatchOptions
    { nboMaxBatchSize = 100,
      nboMaxWaitTime = 2.0 -- 2 seconds
    }

-- | A request to check narinfo for store paths
data BatchRequest requestId = BatchRequest
  { -- | Unique identifier for this request
    brRequestId :: !requestId,
    -- | Store paths to check
    brStorePaths :: ![StorePath]
  }

-- | Response to a batch request
data BatchResponse = BatchResponse
  { -- | All paths in the dependency closure
    brAllPaths :: ![StorePath],
    -- | Paths missing from the cache
    brMissingPaths :: ![StorePath]
  }
  deriving stock (Eq, Show)

-- | Internal state of the batch manager
data BatchState requestId = BatchState
  { -- | Accumulated requests waiting to be processed
    bsPendingRequests :: ![BatchRequest requestId],
    -- | All unique paths from pending requests
    bsAccumulatedPaths :: !(Set StorePath),
    -- | Time when the first request in this batch was added
    bsBatchStartTime :: !(Maybe UTCTime),
    -- | Current timeout TVar (Nothing if no timeout active)
    bsTimeoutVar :: !(Maybe (TVar Bool)),
    -- | Whether the processor should continue running
    bsRunning :: !Bool
  }

-- | Manager for batching narinfo queries
data NarinfoBatchManager requestId = NarinfoBatchManager
  { -- | Configuration
    nbmConfig :: !NarinfoBatchOptions,
    -- | Internal state
    nbmState :: !(TVar (BatchState requestId)),
    -- | Handle to the processor thread
    nbmProcessorThread :: !(MVar (Async ())),
    -- | Callback to handle batch responses
    nbmCallback :: !(requestId -> BatchResponse -> IO ())
  }

-- | Create a new narinfo batch manager
newNarinfoBatchManager :: (MonadIO m) => NarinfoBatchOptions -> (requestId -> BatchResponse -> IO ()) -> m (NarinfoBatchManager requestId)
newNarinfoBatchManager nbmConfig nbmCallback = liftIO $ do
  nbmState <- newTVarIO initialState
  nbmProcessorThread <- newEmptyMVar
  return NarinfoBatchManager {..}
  where
    initialState =
      BatchState
        { bsPendingRequests = [],
          bsAccumulatedPaths = Set.empty,
          bsBatchStartTime = Nothing,
          bsTimeoutVar = Nothing,
          bsRunning = True
        }

-- | Submit a request to the batch manager
submitBatchRequest ::
  (MonadIO m) =>
  NarinfoBatchManager requestId ->
  requestId ->
  [StorePath] ->
  m ()
submitBatchRequest NarinfoBatchManager {nbmConfig, nbmState} requestId storePaths = liftIO $ do
  -- Add request to pending queue
  now <- getCurrentTime

  -- Check if we need to set up a timeout
  batchState <- readTVarIO nbmState
  let isFirstRequest = null (bsPendingRequests batchState)

  updatedBatchState <-
    if isFirstRequest && isNothing (bsTimeoutVar batchState) && nboMaxWaitTime nbmConfig > 0
      then setBatchTimeout (nboMaxWaitTime nbmConfig) batchState
      else return batchState

  atomically $ do
    let newPaths = Set.fromList storePaths
        updatedPaths = bsAccumulatedPaths updatedBatchState <> newPaths
        newRequest = BatchRequest requestId storePaths
        newStartTime = case bsBatchStartTime updatedBatchState of
          Nothing -> Just now
          justTime -> justTime

    writeTVar nbmState $
      updatedBatchState
        { bsPendingRequests = newRequest : bsPendingRequests updatedBatchState,
          bsAccumulatedPaths = updatedPaths,
          bsBatchStartTime = newStartTime
        }

-- | Start the batch processor thread
startBatchProcessor ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  NarinfoBatchManager requestId ->
  -- | Function to process a batch of paths
  ([StorePath] -> m ([StorePath], [StorePath])) ->
  m ()
startBatchProcessor manager@NarinfoBatchManager {nbmProcessorThread} processBatch = do
  -- Start processor thread
  thread <- Async.async $ runBatchProcessor manager processBatch
  liftIO $ putMVar nbmProcessorThread thread

-- | Stop the batch processor
stopBatchProcessor :: (MonadIO m) => NarinfoBatchManager requestId -> m ()
stopBatchProcessor NarinfoBatchManager {nbmState, nbmProcessorThread} = liftIO $ do
  -- Signal shutdown
  atomically $ modifyTVar' nbmState $ \batchState -> batchState {bsRunning = False}

  -- Wait for processor thread to finish
  thread <- tryTakeMVar nbmProcessorThread
  for_ thread Async.wait

-- | Start a timeout for the current batch
startBatchTimeout :: NominalDiffTime -> IO (TVar Bool)
startBatchTimeout delay = do
  let delayMicros = ceiling (delay * 1000000) -- Convert to microseconds
  registerDelay delayMicros

-- | Update batch state to start tracking a timeout
setBatchTimeout :: NominalDiffTime -> BatchState requestId -> IO (BatchState requestId)
setBatchTimeout delay batchState = do
  timeoutVar <- startBatchTimeout delay
  return batchState {bsTimeoutVar = Just timeoutVar}

-- | Check if the current batch has timed out
isBatchTimedOut :: BatchState requestId -> STM Bool
isBatchTimedOut batchState =
  case bsTimeoutVar batchState of
    Nothing -> return False
    Just timeoutVar -> readTVar timeoutVar

-- | Data representing a batch ready for processing
data ReadyBatch requestId = ReadyBatch
  { rbRequests :: ![BatchRequest requestId],
    rbAllPaths :: ![StorePath],
    rbBatchStartTime :: !(Maybe UTCTime)
  }

-- | Wait for a batch to be ready for processing or shutdown
-- Returns Nothing if shutdown requested, Just batch if ready to process
waitForBatchOrShutdown ::
  NarinfoBatchOptions ->
  TVar (BatchState requestId) ->
  STM (Maybe (ReadyBatch requestId))
waitForBatchOrShutdown config stateVar = do
  batchState <- readTVar stateVar

  -- Check for shutdown first
  if not (bsRunning batchState)
    then return Nothing
    else do
      -- Check if we have any pending requests
      if null (bsPendingRequests batchState)
        then retry -- No work, wait for requests
        else do
          -- We have requests, check if batch is ready
          let pathCount = Set.size (bsAccumulatedPaths batchState)
              sizeReady = pathCount >= nboMaxBatchSize config
              -- If timeout is 0, process immediately
              immediateMode = nboMaxWaitTime config <= 0

          -- Check timeout condition
          timeoutReady <- isBatchTimedOut batchState

          if sizeReady || timeoutReady || immediateMode
            then do
              -- Batch is ready, extract data and clear state
              let requests = reverse (bsPendingRequests batchState)
                  allPaths = Set.toList (bsAccumulatedPaths batchState)
                  startTime = bsBatchStartTime batchState

              -- Clear the batch state
              writeTVar stateVar $
                batchState
                  { bsPendingRequests = [],
                    bsAccumulatedPaths = Set.empty,
                    bsBatchStartTime = Nothing,
                    bsTimeoutVar = Nothing
                  }

              return $
                Just
                  ReadyBatch
                    { rbRequests = requests,
                      rbAllPaths = allPaths,
                      rbBatchStartTime = startTime
                    }
            else retry -- Not ready yet, wait for timeout or more requests

-- | Main batch processor loop
runBatchProcessor ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  NarinfoBatchManager requestId ->
  ([StorePath] -> m ([StorePath], [StorePath])) ->
  m ()
runBatchProcessor manager@NarinfoBatchManager {nbmConfig, nbmState} processBatch = do
  loop
  where
    loop = do
      -- Wait for a batch to be ready or shutdown
      maybeReady <- liftIO $ atomically $ waitForBatchOrShutdown nbmConfig nbmState

      case maybeReady of
        Nothing -> return () -- Shutdown requested
        Just readyBatch -> do
          -- Process the ready batch
          processReadyBatch manager processBatch readyBatch
          loop

-- | Process a ready batch
processReadyBatch ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  NarinfoBatchManager requestId ->
  ([StorePath] -> m ([StorePath], [StorePath])) ->
  ReadyBatch requestId ->
  m ()
processReadyBatch NarinfoBatchManager {nbmCallback} processBatch ReadyBatch {rbRequests, rbAllPaths, rbBatchStartTime} = do
  -- Process the batch if we have paths
  unless (null rbAllPaths) $ do
    processingStartTime <- liftIO getCurrentTime

    -- Log batch statistics
    let requestCount = length rbRequests
        pathCount = length rbAllPaths
        waitTime = case rbBatchStartTime of
          Nothing -> 0
          Just startTime -> processingStartTime `diffUTCTime` startTime

    Katip.logFM Katip.InfoS $
      Katip.ls $
        T.intercalate
          " "
          [ "Processing narinfo batch:",
            show requestCount :: Text,
            "requests,",
            show pathCount :: Text,
            "paths,",
            "waited",
            show waitTime
          ]

    -- Query narinfo for all paths at once
    (allPathsInClosure, missingPaths) <- processBatch rbAllPaths

    processingEndTime <- liftIO getCurrentTime
    let processingTime = processingEndTime `diffUTCTime` processingStartTime
        closureSize = length allPathsInClosure
        missingCount = length missingPaths

    Katip.logFM Katip.InfoS $
      Katip.ls $
        T.intercalate
          " "
          [ "Batch completed in",
            show processingTime <> ":",
            show closureSize :: Text,
            "total paths,",
            show missingCount :: Text,
            "missing"
          ]

    -- Build lookup tables
    let allPathsSet = Set.fromList allPathsInClosure
        missingPathsSet = Set.fromList missingPaths

    -- Respond to each request using the manager's callback
    liftIO $ forM_ rbRequests $ \BatchRequest {brRequestId, brStorePaths} -> do
      -- Filter paths relevant to this request
      let requestPaths = filter (`Set.member` allPathsSet) brStorePaths
          requestMissing = filter (`Set.member` missingPathsSet) brStorePaths
          response = BatchResponse requestPaths requestMissing

      -- Call the manager's callback with request ID and response
      nbmCallback brRequestId response
