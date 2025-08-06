{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Daemon.NarinfoBatchSpec where

import Cachix.Daemon.NarinfoBatch
import Control.Concurrent.STM
import Data.Set qualified as Set
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Hercules.CNix qualified as CNix
import Hercules.CNix.Store (Store, StorePath, withStoreFromURI)
import Katip qualified
import Protolude
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import UnliftIO.Async qualified as Async

-- Create a mock StorePath for testing
mockStorePath :: Store -> Int -> IO StorePath
mockStorePath store i = do
  let pathText = "/nix/store/" <> show i <> "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-mock"
  CNix.parseStorePath store (encodeUtf8 pathText)

-- Test data structure to track batch processor calls
data BatchCall = BatchCall
  { bcPaths :: [StorePath],
    bcTimestamp :: UTCTime
  }
  deriving (Show, Eq)

-- Test data structure to track callback calls
data CallbackCall requestId = CallbackCall
  { ccRequestId :: requestId,
    ccResponse :: BatchResponse,
    ccTimestamp :: UTCTime
  }
  deriving (Show, Eq)

-- Mock batch processor that records calls and returns controlled results
createMockBatchProcessor ::
  TVar [BatchCall] -> -- Record of all batch calls
  TVar [(Set StorePath, Set StorePath)] -> -- Queue of (existing, missing) responses
  [StorePath] ->
  IO ([StorePath], [StorePath])
createMockBatchProcessor callsVar responsesVar inputPaths = do
  now <- getCurrentTime
  let call = BatchCall inputPaths now

  atomically $ modifyTVar' callsVar (call :)

  responses <- readTVarIO responsesVar
  case responses of
    [] -> return ([], inputPaths) -- Default: all missing
    ((existing, missing) : rest) -> do
      atomically $ writeTVar responsesVar rest
      let existingList = filter (`Set.member` existing) inputPaths
          missingList = filter (`Set.member` missing) inputPaths
      -- Return all paths in closure (existing paths) and missing paths
      return (existingList, missingList)

-- Mock callback that records all calls
createMockCallback ::
  TVar [CallbackCall requestId] ->
  requestId ->
  BatchResponse ->
  IO ()
createMockCallback callsVar requestId response = do
  now <- getCurrentTime
  let call = CallbackCall requestId response now
  atomically $ modifyTVar' callsVar (call :)

-- Test context to pass around
data TestContext requestId = TestContext
  { tcManager :: NarinfoBatchManager requestId,
    tcBatchCalls :: TVar [BatchCall],
    tcCallbackCalls :: TVar [CallbackCall requestId],
    tcResponsesQueue :: TVar [(Set StorePath, Set StorePath)]
  }

-- Helper to start batch processor asynchronously with its own Katip context
startBatchProcessorAsync :: NarinfoBatchManager requestId -> ([StorePath] -> IO ([StorePath], [StorePath])) -> IO ()
startBatchProcessorAsync manager batchProcessor = do
  void $ Async.async $ do
    handleScribe <- Katip.mkHandleScribe Katip.ColorIfTerminal stderr (Katip.permitItem Katip.InfoS) Katip.V0
    let makeLogEnv = Katip.registerScribe "stderr" handleScribe Katip.defaultScribeSettings =<< Katip.initLogEnv "test" "test"
    bracket makeLogEnv Katip.closeScribes $ \le ->
      Katip.runKatipContextT le () mempty $ startBatchProcessor manager (liftIO . batchProcessor)

-- Test setup helper that encapsulates common initialization
withTestManager ::
  NarinfoBatchOptions ->
  (TestContext Int -> IO a) ->
  IO a
withTestManager config action = do
  batchCalls <- newTVarIO []
  responsesQueue <- newTVarIO []
  callbackCalls <- newTVarIO []

  let callback = createMockCallback callbackCalls
      batchProcessor = createMockBatchProcessor batchCalls responsesQueue

  manager <- newNarinfoBatchManager config callback
  startBatchProcessorAsync manager batchProcessor

  let testContext =
        TestContext
          { tcManager = manager,
            tcBatchCalls = batchCalls,
            tcCallbackCalls = callbackCalls,
            tcResponsesQueue = responsesQueue
          }

  finally (action testContext) (stopBatchProcessor manager)

spec :: Spec
spec = do
  runIO CNix.init
  describe "TTL cache" $ do
    it "correctly handles expiration" $ do
      now <- getCurrentTime
      let past = addUTCTime (-10) now -- 10 seconds ago
          future = addUTCTime 10 now -- 10 seconds from now
          cache :: TTLCache Text
          cache =
            insertTTLCache "expired" past $
              insertTTLCache "valid" future emptyTTLCache

      -- Expired entry should not be found
      lookupTTLCache now "expired" cache `shouldBe` False
      -- Valid entry should be found
      lookupTTLCache now "valid" cache `shouldBe` True

    it "respects size limits when pruning" $ do
      now <- getCurrentTime
      let future = addUTCTime 60 now
          -- Create cache with 5 entries
          cache :: TTLCache Text
          cache = foldl' (\c i -> insertTTLCache (show i) future c) emptyTTLCache [1 .. 5 :: Integer]

      sizeTTLCache cache `shouldBe` 5

      -- Prune to 3 entries
      let pruned = pruneTTLCacheToSize 3 cache
      sizeTTLCache pruned `shouldBe` 3

  describe "Batching" $ do
    it "triggers batch when size threshold is reached" $ withStoreFromURI "dummy://" $ \store -> do
      path1 <- mockStorePath store 1
      path2 <- mockStorePath store 2
      path3 <- mockStorePath store 3
      let config = defaultNarinfoBatchOptions {nboMaxBatchSize = 2, nboMaxWaitTime = 10} -- Large timeout, small batch
      withTestManager config $ \TestContext {..} -> do
        atomically $ writeTVar tcResponsesQueue [(Set.fromList [path1, path2, path3], Set.empty)]

        -- Submit first request (1 path) - should not trigger
        submitBatchRequest tcManager (1 :: Int) [path1]
        threadDelay 10000

        calls1 <- readTVarIO tcBatchCalls
        length calls1 `shouldBe` 0 -- No batch yet

        -- Submit second request (1 more unique path) - should trigger batch (2 paths total)
        submitBatchRequest tcManager (2 :: Int) [path2]
        threadDelay 20000 -- Wait for batch processing
        calls2 <- readTVarIO tcBatchCalls
        length calls2 `shouldBe` 1 -- One batch triggered
        let batchPaths = case head calls2 of
              Just (BatchCall paths _) -> paths
              Nothing -> panic "Expected batch call"
        Set.fromList batchPaths `shouldBe` Set.fromList [path1, path2]

        -- Verify both requests got responses
        callbacks <- readTVarIO tcCallbackCalls
        length callbacks `shouldBe` 2

    it "triggers batch when timeout is reached" $ withStoreFromURI "dummy://" $ \store -> do
      path1 <- mockStorePath store 1
      let config = defaultNarinfoBatchOptions {nboMaxBatchSize = 100, nboMaxWaitTime = 0.05} -- Small timeout, large batch
      withTestManager config $ \TestContext {..} -> do
        atomically $ writeTVar tcResponsesQueue [(Set.fromList [path1], Set.empty)]

        -- Submit request that won't reach size threshold
        submitBatchRequest tcManager (1 :: Int) [path1]

        -- Wait for timeout to trigger
        threadDelay 60000 -- 60ms > 50ms timeout
        calls <- readTVarIO tcBatchCalls
        length calls `shouldBe` 1 -- Batch triggered by timeout
        callbacks <- readTVarIO tcCallbackCalls
        length callbacks `shouldBe` 1 -- Request got response
    it "processes immediately when timeout is zero" $ withStoreFromURI "dummy://" $ \store -> do
      path1 <- mockStorePath store 1
      let config = defaultNarinfoBatchOptions {nboMaxBatchSize = 100, nboMaxWaitTime = 0} -- Immediate mode
      withTestManager config $ \TestContext {..} -> do
        atomically $ writeTVar tcResponsesQueue [(Set.fromList [path1], Set.empty)]

        submitBatchRequest tcManager (1 :: Int) [path1]
        threadDelay 20000 -- Short wait
        calls <- readTVarIO tcBatchCalls
        length calls `shouldBe` 1 -- Processed immediately
        callbacks <- readTVarIO tcCallbackCalls
        length callbacks `shouldBe` 1

    it "only caches existing paths, not missing ones" $ withStoreFromURI "dummy://" $ \store -> do
      path1 <- mockStorePath store 1
      path2 <- mockStorePath store 2
      path3 <- mockStorePath store 3
      let config = defaultNarinfoBatchOptions {nboMaxWaitTime = 0}
      withTestManager config $ \TestContext {..} -> do
        let existingPaths = Set.fromList [path1, path2]
            missingPaths = Set.fromList [path3]
        atomically $ writeTVar tcResponsesQueue [(existingPaths, missingPaths)]

        -- Submit all three paths
        submitBatchRequest tcManager (1 :: Int) [path1, path2, path3]
        threadDelay 20000

        -- Check what's in cache - only existing paths should be cached
        cached1 <- lookupCache tcManager path1
        cached2 <- lookupCache tcManager path2
        cached3 <- lookupCache tcManager path3

        cached1 `shouldBe` True -- Existing path cached
        cached2 `shouldBe` True -- Existing path cached
        cached3 `shouldBe` False -- Missing path not cached
    it "bypasses batch processor for cached paths" $ withStoreFromURI "dummy://" $ \store -> do
      path1 <- mockStorePath store 1
      path2 <- mockStorePath store 2
      let config = defaultNarinfoBatchOptions {nboMaxWaitTime = 0}
      withTestManager config $ \TestContext {..} -> do
        atomically $ writeTVar tcResponsesQueue [(Set.fromList [path1], Set.empty), (Set.fromList [path2], Set.empty)]

        -- First request - path1 will be cached
        submitBatchRequest tcManager (1 :: Int) [path1]
        threadDelay 20000

        -- Second request - path1 from cache, path2 goes to batch
        submitBatchRequest tcManager (2 :: Int) [path1, path2]
        threadDelay 20000

        -- Should have 2 batch calls (one for each unique uncached path)
        calls <- readTVarIO tcBatchCalls
        length calls `shouldBe` 2

        -- Second batch should only contain path2
        let secondBatchPaths = case head calls of
              Just (BatchCall paths _) -> paths
              Nothing -> panic "Expected batch call"
        secondBatchPaths `shouldBe` [path2]

        -- Both requests should have gotten responses
        callbacks <- readTVarIO tcCallbackCalls
        length callbacks `shouldBe` 2

        -- Second response should contain both paths (path1 from cache + path2 from batch)
        let secondResponse = case head callbacks of
              Just (CallbackCall _ response _) -> response
              Nothing -> panic "Expected callback call"
        Set.fromList (brAllPaths secondResponse) `shouldBe` Set.fromList [path1, path2]

    it "distributes correct paths to each request" $ withStoreFromURI "dummy://" $ \store -> do
      path1 <- mockStorePath store 1
      path2 <- mockStorePath store 2
      path3 <- mockStorePath store 3
      path4 <- mockStorePath store 4
      path5 <- mockStorePath store 5
      path6 <- mockStorePath store 6
      let config = defaultNarinfoBatchOptions {nboMaxWaitTime = 0}
      withTestManager config $ \TestContext {..} -> do
        -- Setup: path1,3,5 exist; path2,4,6 missing
        let existingPaths = Set.fromList [path1, path3, path5]
            missingPaths = Set.fromList [path2, path4, path6]
        atomically $ writeTVar tcResponsesQueue [(existingPaths, missingPaths)]

        -- Request 1: paths 1,2,3
        submitBatchRequest tcManager (1 :: Int) [path1, path2, path3]
        -- Request 2: paths 3,4,5 (path 3 overlaps)
        submitBatchRequest tcManager (2 :: Int) [path3, path4, path5]

        threadDelay 20000

        callbacks <- readTVarIO tcCallbackCalls
        length callbacks `shouldBe` 2

        -- Find responses by request ID
        let findResponse rid = find (\(CallbackCall r _ _) -> r == rid) callbacks
        Just (CallbackCall _ response1 _) <- return $ findResponse 1
        Just (CallbackCall _ response2 _) <- return $ findResponse 2

        -- Request 1 should get: existing=[1,3], missing=[2]
        Set.fromList (brAllPaths response1) `shouldBe` Set.fromList [path1, path3]
        Set.fromList (brMissingPaths response1) `shouldBe` Set.fromList [path2]

        -- Request 2 should get: existing=[3,5], missing=[4]
        Set.fromList (brAllPaths response2) `shouldBe` Set.fromList [path3, path5]
        Set.fromList (brMissingPaths response2) `shouldBe` Set.fromList [path4]

    it "deduplicates paths across requests in same batch" $ withStoreFromURI "dummy://" $ \store -> do
      path1 <- mockStorePath store 1
      path2 <- mockStorePath store 2
      let config = defaultNarinfoBatchOptions {nboMaxBatchSize = 3, nboMaxWaitTime = 0.1}
      withTestManager config $ \TestContext {..} -> do
        atomically $ writeTVar tcResponsesQueue [(Set.fromList [path1, path2], Set.empty)]

        -- Submit overlapping requests that will be batched together
        submitBatchRequest tcManager (1 :: Int) [path1, path2] -- paths 1,2
        submitBatchRequest tcManager (2 :: Int) [path2, path1] -- paths 2,1 (same, different order)
        threadDelay 120000 -- Wait 120ms, longer than 100ms timeout
        calls <- readTVarIO tcBatchCalls
        length calls `shouldBe` 1

        -- Batch should contain deduplicated paths
        let batchPaths = case head calls of
              Just (BatchCall paths _) -> paths
              Nothing -> panic "Expected batch call"
        Set.fromList batchPaths `shouldBe` Set.fromList [path1, path2]
        length batchPaths `shouldBe` 2 -- No duplicates

        -- Both requests should still get responses
        callbacks <- readTVarIO tcCallbackCalls
        length callbacks `shouldBe` 2
