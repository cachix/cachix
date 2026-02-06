module Daemon.PushManagerSpec where

import Cachix.Client.Env qualified as Env
import Cachix.Client.OptionsParser (defaultPushOptions)
import Cachix.Client.Push (PushSecret (PushToken))
import Cachix.Daemon.Log qualified as Log
import Cachix.Daemon.NarinfoQuery (defaultNarinfoQueryOptions)
import Cachix.Daemon.Protocol qualified as Protocol
import Cachix.Daemon.Push qualified as Daemon.Push
import Cachix.Daemon.PushManager
import Cachix.Daemon.PushManager.PushJob qualified as PushJob
import Cachix.Daemon.TaskQueue (closeTaskQueue)
import Cachix.Daemon.Types.PushEvent (PushEvent (..), PushEventMessage (..))
import Cachix.Daemon.Types.PushManager
import Cachix.Types.BinaryCache qualified as BinaryCache
import Cachix.Types.Permission (Permission (Write))
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM.TVar
import Control.Monad (fail)
import Control.Retry (defaultRetryStatus)
import Data.IORef
import Data.Set qualified as Set
import Data.Time (diffUTCTime, getCurrentTime)
import Hercules.CNix qualified as CNix
import Protolude
import Servant.Auth.Client (Token (Token))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

instance MonadFail PushManager where
  fail msg = liftIO (expectationFailure msg) >> mzero

spec :: Spec
spec = do
  describe "push job" $ do
    it "starts in the queued state" $ do
      let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"], Protocol.subscribeToUpdates = False}
      pushJob <- PushJob.new request
      PushJob.status pushJob `shouldBe` Queued

    it "can be resolved" $ do
      let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"], Protocol.subscribeToUpdates = False}
          pathSet = Set.fromList ["foo", "bar"]
          closure = PushJob.ResolvedClosure pathSet pathSet
      initPushJob <- PushJob.new request
      timestamp <- getCurrentTime
      let pushJob = PushJob.populateQueue closure timestamp initPushJob
      PushJob.status pushJob `shouldBe` Running
      PushJob.queue pushJob `shouldBe` pathSet
      PushJob.result pushJob `shouldBe` mempty

    it "marks paths as pushed" $
      do
        let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"], Protocol.subscribeToUpdates = False}
            pathSet = Set.fromList ["foo", "bar"]
            closure = PushJob.ResolvedClosure pathSet pathSet
        timestamp <- getCurrentTime

        initPushJob <- PushJob.new request

        let pushJob =
              initPushJob
                & PushJob.populateQueue closure timestamp
                & PushJob.markStorePathPushed "foo"
        PushJob.status pushJob `shouldBe` Running
        PushJob.queue pushJob `shouldBe` Set.fromList ["bar"]
        PushJob.result pushJob
          `shouldBe` PushJob.PushResult
            { PushJob.prFailedPaths = mempty,
              PushJob.prPushedPaths = Set.fromList ["foo"],
              PushJob.prSkippedPaths = mempty
            }

    it "marks paths as failed" $
      do
        let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"], Protocol.subscribeToUpdates = False}
            pathSet = Set.fromList ["foo", "bar"]
            closure = PushJob.ResolvedClosure pathSet pathSet

        timestamp <- getCurrentTime
        initPushJob <- PushJob.new request
        let pushJob =
              initPushJob
                & PushJob.populateQueue closure timestamp
                & PushJob.markStorePathFailed "foo"
        PushJob.status pushJob `shouldBe` Running
        PushJob.queue pushJob `shouldBe` Set.fromList ["bar"]
        PushJob.result pushJob
          `shouldBe` PushJob.PushResult
            { PushJob.prFailedPaths = Set.fromList ["foo"],
              PushJob.prPushedPaths = mempty,
              PushJob.prSkippedPaths = mempty
            }

    it "unmark paths as failed after successful retry" $
      do
        let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"], Protocol.subscribeToUpdates = False}
            pathSet = Set.fromList ["foo", "bar"]
            closure = PushJob.ResolvedClosure pathSet pathSet

        timestamp <- getCurrentTime
        initPushJob <- PushJob.new request
        let pushJob =
              initPushJob
                & PushJob.populateQueue closure timestamp
                & PushJob.markStorePathFailed "foo"
                & PushJob.markStorePathPushed "foo"
        PushJob.status pushJob `shouldBe` Running
        PushJob.result pushJob
          `shouldBe` PushJob.PushResult
            { PushJob.prFailedPaths = mempty,
              PushJob.prPushedPaths = Set.fromList ["foo"],
              PushJob.prSkippedPaths = mempty
            }

  describe "push manager" $ do
    it "queues push jobs " $ inPushManager $ do
      let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"], Protocol.subscribeToUpdates = False}
      Just pushId <- addPushJobFromRequest request
      Just pushJob <- lookupPushJob pushId
      liftIO $ do
        PushJob.pushId pushJob `shouldBe` pushId
        PushJob.pushRequest pushJob `shouldBe` request

    it "manages the lifecycle of a push job" $ inPushManager $ do
      let paths = ["bar", "foo"]

      let pushRequest = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}
      Just pushId <- addPushJobFromRequest pushRequest

      let pathSet = Set.fromList paths
          closure = PushJob.ResolvedClosure pathSet pathSet
      resolvePushJob pushId closure

      withPushJob pushId $ \pushJob -> liftIO $ do
        PushJob.status pushJob `shouldBe` Running
        PushJob.startedAt pushJob `shouldSatisfy` isJust

      forM_ paths $ \path -> do
        pushStorePathAttempt path 1 defaultRetryStatus
        pushStorePathDone path

      withPushJob pushId $ \pushJob -> liftIO $ do
        PushJob.status pushJob `shouldBe` Completed
        PushJob.completedAt pushJob `shouldSatisfy` isJust
        PushJob.result pushJob
          `shouldBe` PushResult
            { prFailedPaths = mempty,
              prPushedPaths = Set.fromList paths,
              prSkippedPaths = mempty
            }

    it "does not create jobs when the task queue is closed" $ withPushManager $ \pm -> do
      runPushManager pm $ do
        queue <- asks pmTaskQueue
        liftIO $ atomically $ closeTaskQueue queue

      let request = Protocol.PushRequest {Protocol.storePaths = ["foo"], Protocol.subscribeToUpdates = False}
      pushJob <- runPushManager pm $ newPushJob request
      didQueue <- runPushManager pm $ addPushJob pushJob
      didQueue `shouldBe` False

      pendingCount <- runPushManager pm pendingJobCount
      pendingCount `shouldBe` 0

      mjob <- runPushManager pm $ lookupPushJob (PushJob.pushId pushJob)
      mjob `shouldBe` Nothing

    it "does not double-decrement pending count on concurrent completion" $ withPushManager $ \pm -> do
      let paths = ["a", "b"]

      Just pushId <- runPushManager pm $ do
        let request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}
        addPushJobFromRequest request

      runPushManager pm $ do
        let pathSet = Set.fromList paths
            closure = PushJob.ResolvedClosure pathSet pathSet
        resolvePushJob pushId closure

      concurrently_
        (runPushManager pm $ pushStorePathDone "a")
        (runPushManager pm $ pushStorePathDone "b")

      pendingCount <- runPushManager pm pendingJobCount
      pendingCount `shouldBe` 0

      mjob <- runPushManager pm (lookupPushJob pushId)
      mjob `shouldBe` Nothing

    describe "graceful shutdown" $ do
      it "shuts down with no jobs" $
        withPushManager $
          stopPushManager timeoutOptions

      it "shuts down after jobs complete" $ withPushManager $ \pm -> do
        let paths = ["foo"]
        let longTimeoutOptions = TimeoutOptions {toTimeout = 1.0, toPollingInterval = 0.1}

        Just _ <- runPushManager pm $ do
          let request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}
          pushId <- addPushJobFromRequest request
          let pathSet = Set.fromList paths
              closure = PushJob.ResolvedClosure pathSet pathSet
          for_ pushId $ \pid -> resolvePushJob pid closure
          return pushId

        startTime <- getCurrentTime
        concurrently_ (stopPushManager longTimeoutOptions pm) $
          runPushManager pm $
            for_ paths pushStorePathDone
        endTime <- getCurrentTime

        let elapsed = diffUTCTime endTime startTime
        elapsed `shouldSatisfy` (< 0.5)

      it "shuts down on job stall" $
        withPushManager $ \pm -> do
          _ <- runPushManager pm $ do
            let request = Protocol.PushRequest {Protocol.storePaths = ["foo"], Protocol.subscribeToUpdates = False}
            addPushJobFromRequest request

          stopPushManager timeoutOptions pm

    describe "queued store path counter" $ do
      it "increments on resolve and decrements as paths complete" $ withPushManager $ \pm -> do
        let paths = ["a", "b", "c"]
            request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}

        Just pushId <- runPushManager pm $ addPushJobFromRequest request

        count0 <- runPushManager pm queuedStorePathCount
        count0 `shouldBe` 0

        runPushManager pm $ do
          let pathSet = Set.fromList paths
              closure = PushJob.ResolvedClosure pathSet pathSet
          resolvePushJob pushId closure

        count1 <- runPushManager pm queuedStorePathCount
        count1 `shouldBe` 3

        runPushManager pm $ pushStorePathDone "a"
        count2 <- runPushManager pm queuedStorePathCount
        count2 `shouldBe` 2

        runPushManager pm $ pushStorePathDone "b"
        count3 <- runPushManager pm queuedStorePathCount
        count3 `shouldBe` 1

        runPushManager pm $ pushStorePathDone "c"
        count4 <- runPushManager pm queuedStorePathCount
        count4 `shouldBe` 0

      it "decrements on failed paths" $ withPushManager $ \pm -> do
        let paths = ["a", "b"]
            request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}

        Just pushId <- runPushManager pm $ addPushJobFromRequest request

        runPushManager pm $ do
          let pathSet = Set.fromList paths
              closure = PushJob.ResolvedClosure pathSet pathSet
          resolvePushJob pushId closure

        count0 <- runPushManager pm queuedStorePathCount
        count0 `shouldBe` 2

        runPushManager pm $ pushStorePathFailed "a" "error"
        count1 <- runPushManager pm queuedStorePathCount
        count1 `shouldBe` 1

        runPushManager pm $ pushStorePathDone "b"
        count2 <- runPushManager pm queuedStorePathCount
        count2 `shouldBe` 0

      it "stays zero when all paths are skipped" $ withPushManager $ \pm -> do
        let paths = ["a", "b"]
            request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}

        Just pushId <- runPushManager pm $ addPushJobFromRequest request

        runPushManager pm $ do
          let allPaths = Set.fromList paths
              closure = PushJob.ResolvedClosure allPaths Set.empty
          resolvePushJob pushId closure

        count <- runPushManager pm queuedStorePathCount
        count `shouldBe` 0

        mjob <- runPushManager pm $ lookupPushJob pushId
        mjob `shouldBe` Nothing

      it "tracks paths across multiple jobs sharing a store path" $ withPushManager $ \pm -> do
        let request1 = Protocol.PushRequest {Protocol.storePaths = ["shared", "unique1"], Protocol.subscribeToUpdates = False}
            request2 = Protocol.PushRequest {Protocol.storePaths = ["shared", "unique2"], Protocol.subscribeToUpdates = False}

        Just pushId1 <- runPushManager pm $ addPushJobFromRequest request1
        Just pushId2 <- runPushManager pm $ addPushJobFromRequest request2

        runPushManager pm $ do
          let pathSet1 = Set.fromList ["shared", "unique1"]
              closure1 = PushJob.ResolvedClosure pathSet1 pathSet1
          resolvePushJob pushId1 closure1

          let pathSet2 = Set.fromList ["shared", "unique2"]
              closure2 = PushJob.ResolvedClosure pathSet2 pathSet2
          resolvePushJob pushId2 closure2

        count0 <- runPushManager pm queuedStorePathCount
        count0 `shouldBe` 4

        -- "shared" is completed for both jobs at once
        runPushManager pm $ pushStorePathDone "shared"
        count1 <- runPushManager pm queuedStorePathCount
        count1 `shouldBe` 2

        runPushManager pm $ pushStorePathDone "unique1"
        count2 <- runPushManager pm queuedStorePathCount
        count2 `shouldBe` 1

        runPushManager pm $ pushStorePathDone "unique2"
        count3 <- runPushManager pm queuedStorePathCount
        count3 `shouldBe` 0

    describe "failed jobs index" $ do
      it "reports jobs that failed before queue population" $ withPushManager $ \pm -> do
        let request = Protocol.PushRequest {Protocol.storePaths = ["a"], Protocol.subscribeToUpdates = False}

        Just pushId <- runPushManager pm $ addPushJobFromRequest request

        runPushManager pm $ failPushJob pushId

        count <- runPushManager pm queuedStorePathCount
        count `shouldBe` 0

        failed <- runPushManager pm getFailedPushJobs
        map PushJob.pushId failed `shouldBe` [pushId]

      it "decrements pending count when failing before resolve" $ withPushManager $ \pm -> do
        let request = Protocol.PushRequest {Protocol.storePaths = ["a"], Protocol.subscribeToUpdates = False}

        Just pushId <- runPushManager pm $ addPushJobFromRequest request
        count0 <- runPushManager pm pendingJobCount
        count0 `shouldBe` 1

        runPushManager pm $ failPushJob pushId
        count1 <- runPushManager pm pendingJobCount
        count1 `shouldBe` 0

      it "cleans up failed jobs after path-level completion" $ withPushManager $ \pm -> do
        let paths = ["a", "b"]
            request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}

        Just pushId <- runPushManager pm $ addPushJobFromRequest request

        runPushManager pm $ do
          let pathSet = Set.fromList paths
              closure = PushJob.ResolvedClosure pathSet pathSet
          resolvePushJob pushId closure

        runPushManager pm $ pushStorePathFailed "a" "error"
        runPushManager pm $ pushStorePathDone "b"

        failed <- runPushManager pm getFailedPushJobs
        failed `shouldBe` []

        mjob <- runPushManager pm $ lookupPushJob pushId
        mjob `shouldBe` Nothing

      it "does not report successful jobs as failed" $ withPushManager $ \pm -> do
        let paths = ["a", "b"]
            request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}

        Just pushId <- runPushManager pm $ addPushJobFromRequest request

        runPushManager pm $ do
          let pathSet = Set.fromList paths
              closure = PushJob.ResolvedClosure pathSet pathSet
          resolvePushJob pushId closure

        runPushManager pm $ pushStorePathDone "a"
        runPushManager pm $ pushStorePathDone "b"

        failed <- runPushManager pm getFailedPushJobs
        failed `shouldBe` []

      it "correctly reports only failed jobs after mixed outcomes" $ withPushManager $ \pm -> do
        let request1 = Protocol.PushRequest {Protocol.storePaths = ["a"], Protocol.subscribeToUpdates = False}
            request2 = Protocol.PushRequest {Protocol.storePaths = ["b", "c"], Protocol.subscribeToUpdates = False}

        Just pushId1 <- runPushManager pm $ addPushJobFromRequest request1
        Just pushId2 <- runPushManager pm $ addPushJobFromRequest request2

        -- Job 1: closure-level failure
        runPushManager pm $ failPushJob pushId1

        -- Job 2: resolve and complete all paths (success)
        runPushManager pm $ do
          let pathSet2 = Set.fromList ["b", "c"]
              closure2 = PushJob.ResolvedClosure pathSet2 pathSet2
          resolvePushJob pushId2 closure2

        runPushManager pm $ pushStorePathDone "b"
        runPushManager pm $ pushStorePathDone "c"

        failed <- runPushManager pm getFailedPushJobs
        map PushJob.pushId failed `shouldBe` [pushId1]

    describe "concurrent path completion" $ do
      it "handles concurrent pushStorePathDone and pushStorePathFailed" $ withPushManager $ \pm -> do
        let paths = ["a", "b"]
            request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}

        Just pushId <- runPushManager pm $ addPushJobFromRequest request

        runPushManager pm $ do
          let pathSet = Set.fromList paths
              closure = PushJob.ResolvedClosure pathSet pathSet
          resolvePushJob pushId closure

        concurrently_
          (runPushManager pm $ pushStorePathDone "a")
          (runPushManager pm $ pushStorePathFailed "b" "error")

        count <- runPushManager pm queuedStorePathCount
        count `shouldBe` 0

        pendingCount <- runPushManager pm pendingJobCount
        pendingCount `shouldBe` 0

        mjob <- runPushManager pm $ lookupPushJob pushId
        mjob `shouldBe` Nothing

    describe "store path index" $ do
      it "ignores completion for unknown store paths" $ withPushManager $ \pm -> do
        let paths = ["a"]
            request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}

        Just pushId <- runPushManager pm $ addPushJobFromRequest request

        runPushManager pm $ do
          let pathSet = Set.fromList paths
              closure = PushJob.ResolvedClosure pathSet pathSet
          resolvePushJob pushId closure

        count0 <- runPushManager pm queuedStorePathCount
        count0 `shouldBe` 1

        runPushManager pm $ pushStorePathDone "missing"

        count1 <- runPushManager pm queuedStorePathCount
        count1 `shouldBe` 1

        mjob <- runPushManager pm $ lookupPushJob pushId
        mjob `shouldSatisfy` isJust

      it "is idempotent when completing a path twice" $ withPushManager $ \pm -> do
        let paths = ["a"]
            request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = False}

        Just pushId <- runPushManager pm $ addPushJobFromRequest request

        runPushManager pm $ do
          let pathSet = Set.fromList paths
              closure = PushJob.ResolvedClosure pathSet pathSet
          resolvePushJob pushId closure

        runPushManager pm $ pushStorePathDone "a"
        pendingCount0 <- runPushManager pm pendingJobCount
        pendingCount0 `shouldBe` 0

        runPushManager pm $ pushStorePathDone "a"
        pendingCount1 <- runPushManager pm pendingJobCount
        pendingCount1 `shouldBe` 0

    describe "push events" $ do
      it "sends store path events to all jobs and clears the index" $ do
        ((pushId1, pushId2), events) <- withPushManagerWithEvents $ \pm -> do
          let request1 = Protocol.PushRequest {Protocol.storePaths = ["shared"], Protocol.subscribeToUpdates = False}
              request2 = Protocol.PushRequest {Protocol.storePaths = ["shared"], Protocol.subscribeToUpdates = False}

          Just pushId1 <- runPushManager pm $ addPushJobFromRequest request1
          Just pushId2 <- runPushManager pm $ addPushJobFromRequest request2

          runPushManager pm $ do
            let pathSet = Set.fromList ["shared"]
                closure = PushJob.ResolvedClosure pathSet pathSet
            resolvePushJob pushId1 closure
            resolvePushJob pushId2 closure

          runPushManager pm $ do
            pushStorePathAttempt "shared" 1 defaultRetryStatus
            pushStorePathDone "shared"
            pushStorePathAttempt "shared" 1 defaultRetryStatus

          return (pushId1, pushId2)

        let attempts =
              [ eventPushId
              | PushEvent {eventPushId, eventMessage = PushStorePathAttempt path _ _} <- events,
                path == "shared"
              ]
            dones =
              [ eventPushId
              | PushEvent {eventPushId, eventMessage = PushStorePathDone path} <- events,
                path == "shared"
              ]

        attempts `shouldMatchList` [pushId1, pushId2]
        dones `shouldMatchList` [pushId1, pushId2]

      it "emits started and finished events" $ do
        (pushId, events) <- withPushManagerWithEvents $ \pm -> do
          let request = Protocol.PushRequest {Protocol.storePaths = ["a"], Protocol.subscribeToUpdates = False}

          Just pushId <- runPushManager pm $ addPushJobFromRequest request

          runPushManager pm $ do
            let pathSet = Set.fromList ["a"]
                closure = PushJob.ResolvedClosure pathSet pathSet
            resolvePushJob pushId closure
            pushStorePathDone "a"

          return pushId

        let started =
              [ eventPushId
              | PushEvent {eventPushId, eventMessage = PushStarted} <- events
              ]
            finished =
              [ eventPushId
              | PushEvent {eventPushId, eventMessage = PushFinished} <- events
              ]

        started `shouldBe` [pushId]
        finished `shouldBe` [pushId]

      it "emits progress events for indexed paths" $ do
        (pushId, events) <- withPushManagerWithEvents $ \pm -> do
          let request = Protocol.PushRequest {Protocol.storePaths = ["a"], Protocol.subscribeToUpdates = False}

          Just pushId <- runPushManager pm $ addPushJobFromRequest request

          runPushManager pm $ do
            let pathSet = Set.fromList ["a"]
                closure = PushJob.ResolvedClosure pathSet pathSet
            resolvePushJob pushId closure
            pushStorePathProgress "a" 10 5

          return pushId

        let progresses =
              [ eventPushId
              | PushEvent {eventPushId, eventMessage = PushStorePathProgress path _ _} <- events,
                path == "a"
              ]

        progresses `shouldBe` [pushId]

  describe "STM" $
    describe "timeout" $ do
      it "times out a transaction after n seconds" $ do
        timestamp <- newTVarIO =<< getCurrentTime
        atomicallyWithTimeout timeoutOptions timestamp retry

withPushManager :: (PushManagerEnv -> IO a) -> IO a
withPushManager f = do
  CNix.init
  withTempStore $ \store -> do
    logger <- liftIO $ Log.new "daemon" Nothing Log.Debug
    cachixOptions <- Env.defaultCachixOptions
    clientEnv <- Env.createClientEnv cachixOptions
    let binaryCache = newBinaryCache "test"
        pushSecret = PushToken (Token "test")
        pushOptions = defaultPushOptions
        batchOptions = defaultNarinfoQueryOptions
        pushParams = Daemon.Push.newPushParams store clientEnv binaryCache pushSecret pushOptions
    newPushManagerEnv pushOptions batchOptions pushParams mempty logger >>= f

withPushManagerWithEvents :: (PushManagerEnv -> IO a) -> IO (a, [PushEvent])
withPushManagerWithEvents f = do
  eventsRef <- newIORef []
  let onPushEvent _ event =
        atomicModifyIORef' eventsRef (\events -> (event : events, ()))

  CNix.init
  withTempStore $ \store -> do
    logger <- liftIO $ Log.new "daemon" Nothing Log.Debug
    cachixOptions <- Env.defaultCachixOptions
    clientEnv <- Env.createClientEnv cachixOptions
    let binaryCache = newBinaryCache "test"
        pushSecret = PushToken (Token "test")
        pushOptions = defaultPushOptions
        batchOptions = defaultNarinfoQueryOptions
        pushParams = Daemon.Push.newPushParams store clientEnv binaryCache pushSecret pushOptions
    env <- newPushManagerEnv pushOptions batchOptions pushParams onPushEvent logger
    result <- f env
    events <- reverse <$> readIORef eventsRef
    return (result, events)

inPushManager :: PushManager a -> IO a
inPushManager f = withPushManager (`runPushManager` f)

withTempStore :: (CNix.Store -> IO a) -> IO a
withTempStore f =
  withSystemTempDirectory "cnix-test-store" $ \d ->
    CNix.withStoreFromURI (toS d) f

newBinaryCache :: BinaryCache.BinaryCacheName -> BinaryCache.BinaryCache
newBinaryCache name =
  BinaryCache.BinaryCache
    { BinaryCache.name = name,
      BinaryCache.uri = "https://" <> name <> ".cachix.org",
      BinaryCache.isPublic = True,
      BinaryCache.publicSigningKeys = [],
      BinaryCache.githubUsername = "",
      BinaryCache.permission = Write,
      BinaryCache.preferredCompressionMethod = BinaryCache.ZSTD
    }

timeoutOptions :: TimeoutOptions
timeoutOptions = TimeoutOptions {toTimeout = 0.2, toPollingInterval = 0.1}
