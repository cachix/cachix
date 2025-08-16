{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cachix.Daemon.NarinfoQuery
  ( -- * Types
    NarinfoQueryManager,
    QueryRequest (..),
    NarinfoResponse (..),
    NarinfoQueryOptions (..),
    defaultNarinfoQueryOptions,

    -- * Operations
    newNarinfoQueryManager,
    submitQueryRequest,
    startQueryProcessor,
    stopQueryProcessor,
    cleanupStaleEntries,

    -- * Cache operations (for testing)
    lookupCache,
    nqmCache,
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

-- | Configuration for the narinfo query manager
data NarinfoQueryOptions = NarinfoQueryOptions
  { -- | Maximum number of paths to accumulate before processing a query
    nqoMaxBatchSize :: !Int,
    -- | Maximum time to wait before processing a query (in seconds)
    -- Use 0 for immediate processing
    nqoMaxWaitTime :: !NominalDiffTime,
    -- | TTL for cache entries (in seconds)
    -- Use 0 to disable caching
    nqoCacheTTL :: !NominalDiffTime,
    -- | Maximum number of entries in the cache
    -- Use 0 for unlimited
    nqoMaxCacheSize :: !Int
  }
  deriving stock (Eq, Show)

-- | Default configuration with reasonable values
defaultNarinfoQueryOptions :: NarinfoQueryOptions
defaultNarinfoQueryOptions =
  NarinfoQueryOptions
    { nqoMaxBatchSize = 100,
      nqoMaxWaitTime = 0.5, -- 500ms
      nqoCacheTTL = 300.0, -- 5 minutes
      nqoMaxCacheSize = 0 -- Unlimited
    }

-- | A request to check narinfo for store paths
data QueryRequest requestId = QueryRequest
  { -- | Unique identifier for this request
    qrRequestId :: !requestId,
    -- | Store paths to check
    qrStorePaths :: ![StorePath],
    -- | Cached existing paths
    qrCachedPaths :: ![StorePath]
  }

-- | Response to a narinfo query request
data NarinfoResponse = NarinfoResponse
  { -- | All paths in the dependency closure
    nrAllPaths :: !(Set StorePath),
    -- | Paths missing from the cache
    nrMissingPaths :: !(Set StorePath)
  }
  deriving stock (Eq, Show)

-- | Internal state of the query manager
data QueryState requestId = QueryState
  { -- | Accumulated requests waiting to be processed
    qsPendingRequests :: !(Seq (QueryRequest requestId)),
    -- | All unique paths from pending requests
    qsAccumulatedPaths :: !(Set StorePath),
    -- | Time when the first request in this query group was added
    qsQueryStartTime :: !(Maybe UTCTime),
    -- | Current timeout TVar (Nothing if no timeout active)
    qsTimeoutVar :: !(Maybe (TVar Bool)),
    -- | Whether the processor should continue running
    qsRunning :: !Bool
  }

-- | Manager for narinfo queries
data NarinfoQueryManager requestId = NarinfoQueryManager
  { -- | Configuration
    nqmConfig :: !NarinfoQueryOptions,
    -- | Internal state
    nqmState :: !(TVar (QueryState requestId)),
    -- | Handle to the processor thread
    nqmProcessorThread :: !(MVar (Async ())),
    -- | Callback to handle query responses
    nqmCallback :: !(requestId -> NarinfoResponse -> IO ()),
    -- | Cache for path existence information
    nqmCache :: !(TVar NarinfoCache),
    -- | Cached current time for TTL calculations (updated periodically)
    nqmCachedTime :: !(TVar UTCTime),
    -- | Handle to the time refresh thread
    nqmTimeRefreshThread :: !(MVar (Async ()))
  }

-- | Create a new narinfo query manager
newNarinfoQueryManager :: (MonadIO m) => NarinfoQueryOptions -> (requestId -> NarinfoResponse -> IO ()) -> m (NarinfoQueryManager requestId)
newNarinfoQueryManager nqmConfig nqmCallback = liftIO $ do
  nqmState <- newTVarIO initialState
  nqmProcessorThread <- newEmptyMVar
  nqmCache <- newTVarIO TTLCache.empty
  nqmCachedTime <- newTVarIO =<< getCurrentTime
  nqmTimeRefreshThread <- newEmptyMVar
  return NarinfoQueryManager {..}
  where
    initialState =
      QueryState
        { qsPendingRequests = Seq.empty,
          qsAccumulatedPaths = Set.empty,
          qsQueryStartTime = Nothing,
          qsTimeoutVar = Nothing,
          qsRunning = True
        }

-- Cache operations

-- | Check if a path exists in the cache and is not stale
lookupCache :: (MonadIO m) => NarinfoQueryManager requestId -> StorePath -> m Bool
lookupCache NarinfoQueryManager {nqmCache, nqmCachedTime} path = liftIO $ do
  pathHash <- getStorePathHash path
  now <- readTVarIO nqmCachedTime
  cache <- readTVarIO nqmCache
  return $ TTLCache.lookup now pathHash cache

-- | Update cache with new entries (only existing paths)
updateCache :: (MonadIO m) => NarinfoQueryManager requestId -> Set StorePath -> m ()
updateCache NarinfoQueryManager {nqmCache, nqmConfig, nqmCachedTime} paths = liftIO $ do
  -- Convert paths to hashes
  pathHashes <- forM (Set.toList paths) getStorePathHash
  now <- readTVarIO nqmCachedTime
  let ttl = nqoCacheTTL nqmConfig
  when (ttl > 0) $ do
    let expiresAt = addUTCTime ttl now
    atomically $ modifyTVar' nqmCache $ \cache ->
      let cacheWithNewEntries = foldr (`TTLCache.insert` expiresAt) cache pathHashes
          maxSize = nqoMaxCacheSize nqmConfig
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
cleanupStaleEntries :: (MonadIO m) => NarinfoQueryManager requestId -> m ()
cleanupStaleEntries NarinfoQueryManager {nqmCache, nqmCachedTime} = liftIO $ do
  now <- readTVarIO nqmCachedTime
  atomically $ modifyTVar' nqmCache $ TTLCache.cleanupExpired now

-- | Submit a request to the query manager
submitQueryRequest ::
  (MonadIO m) =>
  NarinfoQueryManager requestId ->
  requestId ->
  [StorePath] ->
  m ()
submitQueryRequest manager@NarinfoQueryManager {nqmConfig, nqmState, nqmCachedTime} requestId storePaths = liftIO $ do
  -- Add request to pending queue
  now <- readTVarIO nqmCachedTime

  -- Check cache for each path in a single pass
  (cachedExistingPaths, pathsToQuery') <- partitionM (lookupCache manager) storePaths

  -- Perform the state update atomically and determine if timeout is needed
  needsTimeout <- atomically $ do
    batchState <- readTVar nqmState
    let isFirstRequest = Seq.null (qsPendingRequests batchState)
        needsNewTimeout = isFirstRequest && isNothing (qsTimeoutVar batchState) && nqoMaxWaitTime nqmConfig > 0
        updatedPaths = qsAccumulatedPaths batchState <> Set.fromList pathsToQuery'
        newRequest = QueryRequest requestId storePaths cachedExistingPaths
        newStartTime = case qsQueryStartTime batchState of
          Nothing -> Just now
          justTime -> justTime

    writeTVar nqmState $
      batchState
        { qsPendingRequests = qsPendingRequests batchState Seq.|> newRequest,
          qsAccumulatedPaths = updatedPaths,
          qsQueryStartTime = newStartTime
        }

    return needsNewTimeout

  -- Set up timeout outside of STM if needed
  when needsTimeout $ do
    timeoutVar <- startBatchTimeout (nqoMaxWaitTime nqmConfig)
    atomically $ do
      modifyTVar' nqmState $ \bs -> bs {qsTimeoutVar = Just timeoutVar}

-- | Start the query processor thread
startQueryProcessor ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  NarinfoQueryManager requestId ->
  -- | Function to process a batch of paths
  ([StorePath] -> m ([StorePath], [StorePath])) ->
  m ()
startQueryProcessor manager@NarinfoQueryManager {nqmProcessorThread, nqmTimeRefreshThread} processBatch = do
  -- Start time refresh thread
  timeThread <- Async.async $ runTimeRefreshThread manager
  liftIO $ putMVar nqmTimeRefreshThread timeThread

  -- Start processor thread
  thread <- Async.async $ runQueryProcessor manager processBatch
  liftIO $ putMVar nqmProcessorThread thread

-- | Stop the query processor
stopQueryProcessor :: (MonadIO m) => NarinfoQueryManager requestId -> m ()
stopQueryProcessor NarinfoQueryManager {nqmState, nqmProcessorThread, nqmTimeRefreshThread} = liftIO $ do
  -- Signal shutdown
  atomically $ modifyTVar' nqmState $ \batchState -> batchState {qsRunning = False}

  -- Wait for processor thread to finish
  thread <- tryTakeMVar nqmProcessorThread
  for_ thread Async.wait

  -- Wait for time refresh thread to finish
  timeThread <- tryTakeMVar nqmTimeRefreshThread
  for_ timeThread Async.cancel

-- | Start a timeout for the current batch
startBatchTimeout :: NominalDiffTime -> IO (TVar Bool)
startBatchTimeout delay = do
  let delayMicros = ceiling (delay * 1000000) -- Convert to microseconds
  registerDelay delayMicros

-- | Check if the current query has timed out
isQueryTimedOut :: QueryState requestId -> STM Bool
isQueryTimedOut queryState =
  case qsTimeoutVar queryState of
    Nothing -> return False
    Just timeoutVar -> readTVar timeoutVar

-- | Data representing a batch ready for processing
data ReadyBatch requestId = ReadyBatch
  { rbRequests :: !(Seq (QueryRequest requestId)),
    rbAllPaths :: ![StorePath],
    rbBatchStartTime :: !(Maybe UTCTime)
  }

-- | Wait for a batch to be ready for processing or shutdown
-- Returns Nothing if shutdown requested, Just batch if ready to process
waitForBatchOrShutdown ::
  NarinfoQueryOptions ->
  TVar (QueryState requestId) ->
  STM (Maybe (ReadyBatch requestId))
waitForBatchOrShutdown config stateVar = do
  queryState <- readTVar stateVar

  -- Check for shutdown first
  if not (qsRunning queryState)
    then return Nothing
    else do
      -- Check if we have any pending requests
      if Seq.null (qsPendingRequests queryState)
        then retry -- No work, wait for requests
        else do
          -- We have requests, check if batch is ready
          let pathCount = Set.size (qsAccumulatedPaths queryState)
              sizeReady = pathCount >= nqoMaxBatchSize config
              -- If timeout is 0, process immediately
              immediateMode = nqoMaxWaitTime config <= 0

          -- Check timeout condition
          timeoutReady <- isQueryTimedOut queryState

          if sizeReady || timeoutReady || immediateMode
            then do
              -- Batch is ready, extract data and clear state
              let requests = qsPendingRequests queryState
                  allPaths = Set.toList (qsAccumulatedPaths queryState)
                  startTime = qsQueryStartTime queryState

              -- Clear the batch state
              writeTVar stateVar $
                queryState
                  { qsPendingRequests = Seq.empty,
                    qsAccumulatedPaths = Set.empty,
                    qsQueryStartTime = Nothing,
                    qsTimeoutVar = Nothing
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
runTimeRefreshThread :: (MonadUnliftIO m) => NarinfoQueryManager requestId -> m ()
runTimeRefreshThread NarinfoQueryManager {nqmCachedTime} = do
  loop
  where
    loop = do
      -- Wait 2 seconds between updates
      liftIO $ threadDelay 2_000_000 -- 2 seconds in microseconds

      -- Update cached time
      now <- liftIO getCurrentTime
      liftIO $ atomically $ writeTVar nqmCachedTime now
      loop

-- | Main query processor loop
runQueryProcessor ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  NarinfoQueryManager requestId ->
  ([StorePath] -> m ([StorePath], [StorePath])) ->
  m ()
runQueryProcessor manager@NarinfoQueryManager {nqmConfig, nqmState} processBatch = do
  loop
  where
    loop = do
      -- Wait for a batch to be ready or shutdown
      maybeReady <- liftIO $ atomically $ waitForBatchOrShutdown nqmConfig nqmState

      case maybeReady of
        Nothing -> return () -- Shutdown requested
        Just readyBatch -> do
          -- Process the ready batch
          processReadyBatch manager processBatch readyBatch
          loop

-- | Process a ready batch
processReadyBatch ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  NarinfoQueryManager requestId ->
  ([StorePath] -> m ([StorePath], [StorePath])) ->
  ReadyBatch requestId ->
  m ()
processReadyBatch manager@NarinfoQueryManager {nqmCallback} processBatch ReadyBatch {rbRequests, rbAllPaths, rbBatchStartTime} = do
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
  liftIO $ forM_ rbRequests $ \QueryRequest {qrRequestId, qrStorePaths, qrCachedPaths} -> do
    -- For each request, determine which of its requested paths exist (from API or cache)
    let -- Convert request paths to sets for efficient operations
        qrStorePathsSet = Set.fromList qrStorePaths
        qrCachedPathsSet = Set.fromList qrCachedPaths
        -- Paths that exist: intersection of request paths with all existing paths (API + cached)
        existingPathsFromAPI = qrStorePathsSet `Set.intersection` allPathsSet
        missingPathsFromAPI = qrStorePathsSet `Set.intersection` missingPathsSet
        -- Combine existing paths from API and cache, removing duplicates
        allRequestPaths = existingPathsFromAPI `Set.union` qrCachedPathsSet
        response = NarinfoResponse allRequestPaths missingPathsFromAPI

    -- Call the manager's callback with request ID and response
    nqmCallback qrRequestId response
