{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cachix.Daemon.NarinfoBatch
  ( -- * Types
    NarinfoBatchManager,
    BatchRequest (..),
    BatchResponse (..),
    BatchConfig (..),
    defaultBatchConfig,

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
data BatchConfig = BatchConfig
  { -- | Maximum number of paths to accumulate before triggering a batch
    bcMaxBatchSize :: !Int,
    -- | Maximum time to wait before triggering a batch (in seconds)
    bcMaxWaitTime :: !NominalDiffTime,
    -- | Whether the batch processor is enabled
    bcEnabled :: !Bool
  }
  deriving stock (Eq, Show)

-- | Default configuration with reasonable values
defaultBatchConfig :: BatchConfig
defaultBatchConfig =
  BatchConfig
    { bcMaxBatchSize = 100,
      bcMaxWaitTime = 2.0, -- 2 seconds
      bcEnabled = True
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
    -- | Whether the processor should continue running
    bsRunning :: !Bool
  }

-- | Manager for batching narinfo queries
data NarinfoBatchManager requestId = NarinfoBatchManager
  { -- | Configuration
    nbmConfig :: !BatchConfig,
    -- | Internal state
    nbmState :: !(TVar (BatchState requestId)),
    -- | Condition variable to signal new work
    nbmWorkAvailable :: !(TMVar ()),
    -- | Handle to the processor thread
    nbmProcessorThread :: !(MVar (Async ())),
    -- | Callback to handle batch responses
    nbmCallback :: !(requestId -> BatchResponse -> IO ())
  }

-- | Create a new narinfo batch manager
newNarinfoBatchManager :: (MonadIO m) => BatchConfig -> (requestId -> BatchResponse -> IO ()) -> m (NarinfoBatchManager requestId)
newNarinfoBatchManager nbmConfig nbmCallback = liftIO $ do
  nbmState <- newTVarIO initialState
  nbmWorkAvailable <- newEmptyTMVarIO
  nbmProcessorThread <- newEmptyMVar
  return NarinfoBatchManager {..}
  where
    initialState =
      BatchState
        { bsPendingRequests = [],
          bsAccumulatedPaths = Set.empty,
          bsBatchStartTime = Nothing,
          bsRunning = True
        }

-- | Submit a request to the batch manager
submitBatchRequest ::
  (MonadIO m) =>
  NarinfoBatchManager requestId ->
  requestId ->
  [StorePath] ->
  m ()
submitBatchRequest NarinfoBatchManager {nbmConfig, nbmState, nbmWorkAvailable, nbmCallback} requestId storePaths = liftIO $ do
  if not (bcEnabled nbmConfig)
    then -- Batching disabled, call callback immediately
      nbmCallback requestId $ BatchResponse storePaths []
    else do
      -- Add request to pending queue
      now <- getCurrentTime
      atomically $ do
        modifyTVar' nbmState $ \batchState ->
          let newPaths = Set.fromList storePaths
              updatedPaths = bsAccumulatedPaths batchState <> newPaths
              newRequest = BatchRequest requestId storePaths
              newStartTime = case bsBatchStartTime batchState of
                Nothing -> Just now
                justTime -> justTime
           in batchState
                { bsPendingRequests = newRequest : bsPendingRequests batchState,
                  bsAccumulatedPaths = updatedPaths,
                  bsBatchStartTime = newStartTime
                }

        -- Signal that work is available
        void $ tryPutTMVar nbmWorkAvailable ()

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
stopBatchProcessor NarinfoBatchManager {nbmState, nbmWorkAvailable, nbmProcessorThread} = liftIO $ do
  -- Signal shutdown
  atomically $ do
    modifyTVar' nbmState $ \batchState -> batchState {bsRunning = False}
    void $ tryPutTMVar nbmWorkAvailable ()

  -- Wait for processor thread to finish
  thread <- tryTakeMVar nbmProcessorThread
  for_ thread Async.wait

-- | Main batch processor loop
runBatchProcessor ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  NarinfoBatchManager requestId ->
  ([StorePath] -> m ([StorePath], [StorePath])) ->
  m ()
runBatchProcessor manager@NarinfoBatchManager {nbmConfig, nbmState, nbmWorkAvailable} processBatch = do
  loop
  where
    loop = do
      -- Wait for work or timeout
      shouldProcess <- liftIO $ atomically $ do
        batchState <- readTVar nbmState
        if not (bsRunning batchState)
          then return Nothing -- Shutdown requested
          else
            if null (bsPendingRequests batchState)
              then do
                -- No work, wait for signal
                takeTMVar nbmWorkAvailable
                return $ Just False -- Check again
              else do
                -- We have work, check if we should process
                return $ Just True

      case shouldProcess of
        Nothing -> return () -- Shutdown
        Just False -> loop -- Check again
        Just True -> do
          -- Check if batch is ready
          now <- liftIO getCurrentTime
          ready <- liftIO $ atomically $ do
            batchState <- readTVar nbmState
            let pathCount = Set.size (bsAccumulatedPaths batchState)
                timeElapsed = case bsBatchStartTime batchState of
                  Nothing -> 0
                  Just startTime -> now `diffUTCTime` startTime

            return $
              pathCount >= bcMaxBatchSize nbmConfig
                || timeElapsed >= bcMaxWaitTime nbmConfig

          if ready
            then do
              -- Process the batch
              processPendingBatch manager processBatch
              loop
            else do
              -- Not ready yet, wait a bit
              liftIO $ threadDelay 100000 -- 100ms
              loop

-- | Process all pending requests as a single batch
processPendingBatch ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  NarinfoBatchManager requestId ->
  ([StorePath] -> m ([StorePath], [StorePath])) ->
  m ()
processPendingBatch NarinfoBatchManager {nbmState, nbmCallback} processBatch = do
  -- Extract pending requests
  (requests, allPaths, batchStartTime) <- liftIO $ atomically $ do
    batchState <- readTVar nbmState
    let requests = reverse (bsPendingRequests batchState) -- Process in FIFO order
        allPaths = Set.toList (bsAccumulatedPaths batchState)
        startTime = bsBatchStartTime batchState

    -- Clear the state
    writeTVar nbmState $
      batchState
        { bsPendingRequests = [],
          bsAccumulatedPaths = Set.empty,
          bsBatchStartTime = Nothing
        }

    return (requests, allPaths, startTime)

  -- Process the batch if we have paths
  unless (null allPaths) $ do
    processingStartTime <- liftIO getCurrentTime

    -- Log batch statistics
    let requestCount = length requests
        pathCount = length allPaths
        waitTime = case batchStartTime of
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
    (allPathsInClosure, missingPaths) <- processBatch allPaths

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
    liftIO $ forM_ requests $ \BatchRequest {brRequestId, brStorePaths} -> do
      -- Filter paths relevant to this request
      let requestPaths = filter (`Set.member` allPathsSet) brStorePaths
          requestMissing = filter (`Set.member` missingPathsSet) brStorePaths
          response = BatchResponse requestPaths requestMissing

      -- Call the manager's callback with request ID and response
      nbmCallback brRequestId response
