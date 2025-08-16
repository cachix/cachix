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
    cleanupStaleEntries,

    -- * Cache operations (for testing)
    lookupCache,
    nbmCache,
  )
where

import Cachix.Daemon.TTLCache (TTLCache)
import Cachix.Daemon.TTLCache qualified as TTLCache
import Control.Concurrent.STM
import Control.Monad.Extra (partitionM)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Hercules.CNix.Store (StorePath, getStorePathHash)
import Katip qualified
import Protolude
import UnliftIO.Async qualified as Async

-- | Type alias for the positive narinfo cache.
-- Keys are store path hashes.
type NarinfoCache = TTLCache ByteString

-- | Configuration for the narinfo batch manager
data NarinfoBatchOptions = NarinfoBatchOptions
  { -- | Maximum number of paths to accumulate before triggering a batch
    nboMaxBatchSize :: !Int,
    -- | Maximum time to wait before triggering a batch (in seconds)
    -- Use 0 for immediate processing (no batching)
    nboMaxWaitTime :: !NominalDiffTime,
    -- | TTL for cache entries (in seconds)
    -- Use 0 to disable caching
    nboCacheTTL :: !NominalDiffTime,
    -- | Maximum number of entries in the cache
    -- Use 0 for unlimited
    nboMaxCacheSize :: !Int
  }
  deriving stock (Eq, Show)

-- | Default configuration with reasonable values
defaultNarinfoBatchOptions :: NarinfoBatchOptions
defaultNarinfoBatchOptions =
  NarinfoBatchOptions
    { nboMaxBatchSize = 100,
      nboMaxWaitTime = 0.5, -- 500ms
      nboCacheTTL = 300.0, -- 5 minutes
      nboMaxCacheSize = 0 -- Unlimited
    }

-- | A request to check narinfo for store paths
data BatchRequest requestId = BatchRequest
  { -- | Unique identifier for this request
    brRequestId :: !requestId,
    -- | Store paths to check
    brStorePaths :: ![StorePath],
    -- | Cached existing paths
    brCachedPaths :: ![StorePath]
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
    bsPendingRequests :: !(Seq (BatchRequest requestId)),
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
    nbmCallback :: !(requestId -> BatchResponse -> IO ()),
    -- | Cache for path existence information
    nbmCache :: !(TVar NarinfoCache),
    -- | Cached current time for TTL calculations (updated periodically)
    nbmCachedTime :: !(TVar UTCTime),
    -- | Handle to the time refresh thread
    nbmTimeRefreshThread :: !(MVar (Async ()))
  }

-- | Create a new narinfo batch manager
newNarinfoBatchManager :: (MonadIO m) => NarinfoBatchOptions -> (requestId -> BatchResponse -> IO ()) -> m (NarinfoBatchManager requestId)
newNarinfoBatchManager nbmConfig nbmCallback = liftIO $ do
  nbmState <- newTVarIO initialState
  nbmProcessorThread <- newEmptyMVar
  nbmCache <- newTVarIO TTLCache.empty
  nbmCachedTime <- newTVarIO =<< getCurrentTime
  nbmTimeRefreshThread <- newEmptyMVar
  return NarinfoBatchManager {..}
  where
    initialState =
      BatchState
        { bsPendingRequests = Seq.empty,
          bsAccumulatedPaths = Set.empty,
          bsBatchStartTime = Nothing,
          bsTimeoutVar = Nothing,
          bsRunning = True
        }

-- Cache operations

-- | Check if a path exists in the cache and is not stale
lookupCache :: (MonadIO m) => NarinfoBatchManager requestId -> StorePath -> m Bool
lookupCache NarinfoBatchManager {nbmCache, nbmCachedTime} path = liftIO $ do
  pathHash <- getStorePathHash path
  now <- readTVarIO nbmCachedTime
  cache <- readTVarIO nbmCache
  return $ TTLCache.lookup now pathHash cache

-- | Update cache with new entries (only existing paths)
updateCache :: (MonadIO m) => NarinfoBatchManager requestId -> Set StorePath -> m ()
updateCache NarinfoBatchManager {nbmCache, nbmConfig, nbmCachedTime} paths = liftIO $ do
  -- Convert paths to hashes
  pathHashes <- forM (Set.toList paths) getStorePathHash
  now <- readTVarIO nbmCachedTime
  let ttl = nboCacheTTL nbmConfig
  when (ttl > 0) $ do
    let expiresAt = addUTCTime ttl now
    atomically $ modifyTVar' nbmCache $ \cache ->
      let cacheWithNewEntries = foldr (`TTLCache.insert` expiresAt) cache pathHashes
          maxSize = nboMaxCacheSize nbmConfig
          currentSize = TTLCache.size cacheWithNewEntries
          -- Lazy cleanup: only clean up if cache is getting large (20% over limit)
          cleanupThreshold = if maxSize > 0 then maxSize + (maxSize `div` 5) else maxBound
       in if currentSize > cleanupThreshold
            then
              let cleanedCache = TTLCache.cleanupExpired now cacheWithNewEntries
                  finalCache =
                    if maxSize > 0 && TTLCache.size cleanedCache > maxSize
                      then TTLCache.pruneToSize maxSize cleanedCache
                      else cleanedCache
               in finalCache
            else cacheWithNewEntries

-- | Remove stale entries from cache
cleanupStaleEntries :: (MonadIO m) => NarinfoBatchManager requestId -> m ()
cleanupStaleEntries NarinfoBatchManager {nbmCache, nbmCachedTime} = liftIO $ do
  now <- readTVarIO nbmCachedTime
  atomically $ modifyTVar' nbmCache $ TTLCache.cleanupExpired now

-- | Submit a request to the batch manager
submitBatchRequest ::
  (MonadIO m) =>
  NarinfoBatchManager requestId ->
  requestId ->
  [StorePath] ->
  m ()
submitBatchRequest manager@NarinfoBatchManager {nbmConfig, nbmState, nbmCachedTime} requestId storePaths = liftIO $ do
  -- Add request to pending queue
  now <- readTVarIO nbmCachedTime

  -- Check cache for each path in a single pass
  (cachedExistingPaths, pathsToQuery') <- partitionM (lookupCache manager) storePaths

  -- Perform the state update atomically and determine if timeout is needed
  needsTimeout <- atomically $ do
    batchState <- readTVar nbmState
    let isFirstRequest = Seq.null (bsPendingRequests batchState)
        needsNewTimeout = isFirstRequest && isNothing (bsTimeoutVar batchState) && nboMaxWaitTime nbmConfig > 0
        updatedPaths = bsAccumulatedPaths batchState <> Set.fromList pathsToQuery'
        newRequest = BatchRequest requestId storePaths cachedExistingPaths
        newStartTime = case bsBatchStartTime batchState of
          Nothing -> Just now
          justTime -> justTime

    writeTVar nbmState $
      batchState
        { bsPendingRequests = bsPendingRequests batchState Seq.|> newRequest,
          bsAccumulatedPaths = updatedPaths,
          bsBatchStartTime = newStartTime
        }

    return needsNewTimeout

  -- Set up timeout outside of STM if needed
  when needsTimeout $ do
    timeoutVar <- startBatchTimeout (nboMaxWaitTime nbmConfig)
    atomically $ do
      modifyTVar' nbmState $ \bs -> bs {bsTimeoutVar = Just timeoutVar}

-- | Start the batch processor thread
startBatchProcessor ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  NarinfoBatchManager requestId ->
  -- | Function to process a batch of paths
  ([StorePath] -> m ([StorePath], [StorePath])) ->
  m ()
startBatchProcessor manager@NarinfoBatchManager {nbmProcessorThread, nbmTimeRefreshThread} processBatch = do
  -- Start time refresh thread
  timeThread <- Async.async $ runTimeRefreshThread manager
  liftIO $ putMVar nbmTimeRefreshThread timeThread

  -- Start processor thread
  thread <- Async.async $ runBatchProcessor manager processBatch
  liftIO $ putMVar nbmProcessorThread thread

-- | Stop the batch processor
stopBatchProcessor :: (MonadIO m) => NarinfoBatchManager requestId -> m ()
stopBatchProcessor NarinfoBatchManager {nbmState, nbmProcessorThread, nbmTimeRefreshThread} = liftIO $ do
  -- Signal shutdown
  atomically $ modifyTVar' nbmState $ \batchState -> batchState {bsRunning = False}

  -- Wait for processor thread to finish
  thread <- tryTakeMVar nbmProcessorThread
  for_ thread Async.wait

  -- Wait for time refresh thread to finish
  timeThread <- tryTakeMVar nbmTimeRefreshThread
  for_ timeThread Async.cancel

-- | Start a timeout for the current batch
startBatchTimeout :: NominalDiffTime -> IO (TVar Bool)
startBatchTimeout delay = do
  let delayMicros = ceiling (delay * 1000000) -- Convert to microseconds
  registerDelay delayMicros

-- | Check if the current batch has timed out
isBatchTimedOut :: BatchState requestId -> STM Bool
isBatchTimedOut batchState =
  case bsTimeoutVar batchState of
    Nothing -> return False
    Just timeoutVar -> readTVar timeoutVar

-- | Data representing a batch ready for processing
data ReadyBatch requestId = ReadyBatch
  { rbRequests :: !(Seq (BatchRequest requestId)),
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
      if Seq.null (bsPendingRequests batchState)
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
              let requests = bsPendingRequests batchState
                  allPaths = Set.toList (bsAccumulatedPaths batchState)
                  startTime = bsBatchStartTime batchState

              -- Clear the batch state
              writeTVar stateVar $
                batchState
                  { bsPendingRequests = Seq.empty,
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

-- | Time refresh thread that updates cached time every few seconds
runTimeRefreshThread :: (MonadUnliftIO m) => NarinfoBatchManager requestId -> m ()
runTimeRefreshThread NarinfoBatchManager {nbmCachedTime} = do
  loop
  where
    loop = do
      -- Wait 2 seconds between updates
      liftIO $ threadDelay 2_000_000 -- 2 seconds in microseconds

      -- Update cached time
      now <- liftIO getCurrentTime
      liftIO $ atomically $ writeTVar nbmCachedTime now
      loop

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
processReadyBatch manager@NarinfoBatchManager {nbmCallback} processBatch ReadyBatch {rbRequests, rbAllPaths, rbBatchStartTime} = do
  -- Process the batch if we have paths to query
  (allPathsInClosure, missingPaths) <-
    if null rbAllPaths
      then return ([], [])
      else do
        processingStartTime <- liftIO getCurrentTime

        -- Log batch statistics
        let requestCount = Seq.length rbRequests
            pathCount = length rbAllPaths
            waitTime = case rbBatchStartTime of
              Nothing -> 0
              Just startTime -> processingStartTime `diffUTCTime` startTime

        Katip.logFM Katip.DebugS "Processing narinfo batch"
          & Katip.katipAddContext
            ( Katip.sl "requests" requestCount
                <> Katip.sl "paths" pathCount
                <> Katip.sl "wait_time_s" (show waitTime :: Text)
            )

        -- Query narinfo for all paths at once
        (allPathsInClosure, missingPaths) <- processBatch rbAllPaths

        processingEndTime <- liftIO getCurrentTime
        let processingTime = processingEndTime `diffUTCTime` processingStartTime
            closureSize = length allPathsInClosure
            missingCount = length missingPaths

        Katip.logFM Katip.DebugS "Batch completed"
          & Katip.katipAddContext
            ( Katip.sl "processing_time_s" (show processingTime :: Text)
                <> Katip.sl "total_paths" closureSize
                <> Katip.sl "missing_paths" missingCount
            )
        -- Update cache with results (only cache positive lookups)
        let missingPathsSet = Set.fromList missingPaths
            allPathsSet = Set.fromList allPathsInClosure
            existingPathsSet = allPathsSet `Set.difference` missingPathsSet
        updateCache manager existingPathsSet

        return (allPathsInClosure, missingPaths)

  -- Build lookup tables including cached results
  let allPathsSet = Set.fromList allPathsInClosure
      missingPathsSet = Set.fromList missingPaths

  -- Respond to each request using the manager's callback
  liftIO $ forM_ (toList rbRequests) $ \BatchRequest {brRequestId, brStorePaths, brCachedPaths} -> do
    -- For each request, determine which of its requested paths exist (from API or cache)
    let -- Paths that exist: intersection of request paths with all existing paths (API + cached)
        existingPathsFromAPI = filter (`Set.member` allPathsSet) brStorePaths
        missingPathsFromAPI = filter (`Set.member` missingPathsSet) brStorePaths
        -- Combine existing paths from API and cache, removing duplicates
        allRequestPaths = Set.toList $ Set.fromList (existingPathsFromAPI ++ brCachedPaths)
        allRequestMissing = missingPathsFromAPI
        response = BatchResponse allRequestPaths allRequestMissing

    -- Call the manager's callback with request ID and response
    nbmCallback brRequestId response
