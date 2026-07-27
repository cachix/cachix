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
import Cachix.Daemon.Types.PushEvent (PushEvent (..), PushEventMessage (..))
import Cachix.Daemon.Types.PushManager
import Cachix.Types.BinaryCache qualified as BinaryCache
import Cachix.Types.Permission (Permission (Write))
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.STM.TVar
import Control.Monad (fail)
import Control.Retry (defaultRetryStatus)
import Data.Set qualified as Set
import Data.Time (diffUTCTime, getCurrentTime)
import Hercules.CNix qualified as CNix
import Protolude
import Servant.Auth.Client (Token (Token))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout qualified as Timeout
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

    describe "failing jobs" $ do
      it "notifies subscribers when a job fails before closure resolution" $ do
        events <- newTVarIO []
        withPushManagerOnEvent (recordEvents events) $ \pm -> do
          runPushManager pm $ do
            let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"], Protocol.subscribeToUpdates = True}
            Just pushId <- addPushJobFromRequest request
            failPushJob pushId "closure resolution failed"

            withPushJob pushId $ \pushJob ->
              liftIO $ PushJob.status pushJob `shouldBe` Failed

          messages <- map eventMessage . reverse <$> readTVarIO events
          messages
            `shouldBe` [ PushStorePathFailed "bar" "closure resolution failed",
                         PushStorePathFailed "foo" "closure resolution failed",
                         PushFinished
                       ]

      it "does not emit further events for failed jobs on shutdown" $ do
        events <- newTVarIO []
        withPushManagerOnEvent (recordEvents events) $ \pm -> do
          runPushManager pm $ do
            let request = Protocol.PushRequest {Protocol.storePaths = ["foo"], Protocol.subscribeToUpdates = True}
            Just pushId <- addPushJobFromRequest request
            failPushJob pushId "closure resolution failed"

          eventsAfterFailure <- readTVarIO events
          failed <- runPushManager pm $ failPendingJobs "daemon stopped"
          length failed `shouldBe` 0
          eventsAfterShutdown <- readTVarIO events
          eventsAfterShutdown `shouldBe` eventsAfterFailure

      it "only reports paths that were not pushed when failing a resolved job" $ do
        events <- newTVarIO []
        withPushManagerOnEvent (recordEvents events) $ \pm -> do
          runPushManager pm $ do
            let paths = ["bar", "foo"]
                pathSet = Set.fromList paths
            let request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = True}
            Just pushId <- addPushJobFromRequest request
            resolvePushJob pushId (PushJob.ResolvedClosure pathSet pathSet)
            pushStorePathDone "bar"
            void $ failPendingJobs "daemon stopped"

          messages <- map eventMessage . reverse <$> readTVarIO events
          messages
            `shouldBe` [ PushStarted,
                         PushStorePathDone "bar",
                         PushStorePathFailed "foo" "daemon stopped",
                         PushFinished
                       ]

      it "does not emit events for a failed job when its in-flight paths finish" $ do
        events <- newTVarIO []
        withPushManagerOnEvent (recordEvents events) $ \pm -> do
          runPushManager pm $ do
            let paths = ["bar", "foo"]
                pathSet = Set.fromList paths
            let request = Protocol.PushRequest {Protocol.storePaths = paths, Protocol.subscribeToUpdates = True}
            Just pushId <- addPushJobFromRequest request
            resolvePushJob pushId (PushJob.ResolvedClosure pathSet pathSet)
            failPushJob pushId "daemon stopped"

          eventsAfterFailure <- readTVarIO events
          lastMay (map eventMessage (reverse eventsAfterFailure)) `shouldBe` Just PushFinished

          -- The uploads that were in flight when the job failed report back.
          runPushManager pm $ do
            pushStorePathAttempt "foo" 1 defaultRetryStatus
            pushStorePathProgress "foo" 1 1
            pushStorePathDone "foo"
            pushStorePathFailed "bar" "upload failed"

          eventsAfterUploads <- readTVarIO events
          eventsAfterUploads `shouldBe` eventsAfterFailure

      it "ignores closure resolution for a job that already failed" $ do
        events <- newTVarIO []
        withPushManagerOnEvent (recordEvents events) $ \pm -> do
          pushId <- runPushManager pm $ do
            let request = Protocol.PushRequest {Protocol.storePaths = ["foo"], Protocol.subscribeToUpdates = True}
            Just pushId <- addPushJobFromRequest request
            failPushJob pushId "daemon stopped"
            pure pushId

          eventsAfterFailure <- readTVarIO events
          lastMay (map eventMessage (reverse eventsAfterFailure)) `shouldBe` Just PushFinished

          runPushManager pm $ do
            let pathSet = Set.fromList ["foo"]
            resolvePushJob pushId (PushJob.ResolvedClosure pathSet pathSet)
            pushStorePathDone "foo"

            withPushJob pushId $ \pushJob ->
              liftIO $ PushJob.status pushJob `shouldBe` Failed
            count <- pendingJobCount
            liftIO $ count `shouldBe` 0

          eventsAfterResolve <- readTVarIO events
          eventsAfterResolve `shouldBe` eventsAfterFailure

      it "serializes closure resolution with failure" $ do
        events <- newTVarIO []
        startedEvent <- MVar.newEmptyMVar
        releaseStartedEvent <- MVar.newEmptyMVar
        let onEvent pushId event = do
              recordEvents events pushId event
              when (eventMessage event == PushStarted) $ do
                MVar.putMVar startedEvent ()
                MVar.takeMVar releaseStartedEvent

        withPushManagerOnEvent onEvent $ \pm -> do
          pushId <- runPushManager pm $ do
            let request = Protocol.PushRequest {Protocol.storePaths = ["foo"], Protocol.subscribeToUpdates = True}
            Just pushId <- addPushJobFromRequest request
            pure pushId

          let pathSet = Set.singleton "foo"
              resolve = runPushManager pm $ resolvePushJob pushId (PushJob.ResolvedClosure pathSet pathSet)
              failJob = runPushManager pm $ failPushJob pushId "daemon stopped"
          Async.withAsync resolve $ \resolveThread -> do
            MVar.takeMVar startedEvent
            failureInvoked <- MVar.newEmptyMVar
            Async.withAsync (MVar.putMVar failureInvoked () >> failJob) $ \failureThread -> do
              MVar.takeMVar failureInvoked
              assertStillRunning failureThread
              MVar.putMVar releaseStartedEvent ()
              Async.wait resolveThread
              Async.wait failureThread

          runPushManager pm $ do
            pushStorePathAttempt "foo" 1 defaultRetryStatus
            pushStorePathProgress "foo" 1 1
            pushStorePathDone "foo"

          messages <- map eventMessage . reverse <$> readTVarIO events
          messages
            `shouldBe` [ PushStarted,
                         PushStorePathFailed "foo" "daemon stopped",
                         PushFinished
                       ]

      it "serializes in-flight progress events with failure" $ do
        events <- newTVarIO []
        progressEvent <- MVar.newEmptyMVar
        releaseProgressEvent <- MVar.newEmptyMVar
        let onEvent pushId event = do
              recordEvents events pushId event
              when (isProgressEvent event) $ do
                MVar.putMVar progressEvent ()
                MVar.takeMVar releaseProgressEvent

        withPushManagerOnEvent onEvent $ \pm -> do
          pushId <- runPushManager pm $ do
            let request = Protocol.PushRequest {Protocol.storePaths = ["foo"], Protocol.subscribeToUpdates = True}
                pathSet = Set.singleton "foo"
            Just pushId <- addPushJobFromRequest request
            resolvePushJob pushId (PushJob.ResolvedClosure pathSet pathSet)
            pure pushId

          let failJob = runPushManager pm $ failPushJob pushId "daemon stopped"
          Async.withAsync (runPushManager pm $ pushStorePathProgress "foo" 1 1) $ \progressThread -> do
            MVar.takeMVar progressEvent
            failureInvoked <- MVar.newEmptyMVar
            Async.withAsync (MVar.putMVar failureInvoked () >> failJob) $ \failureThread -> do
              MVar.takeMVar failureInvoked
              assertStillRunning failureThread
              MVar.putMVar releaseProgressEvent ()
              Async.wait progressThread
              Async.wait failureThread

          runPushManager pm $ do
            pushStorePathAttempt "foo" 1 defaultRetryStatus
            pushStorePathDone "foo"

          messages <- map eventMessage . reverse <$> readTVarIO events
          messages
            `shouldBe` [ PushStarted,
                         PushStorePathProgress "foo" 1 1,
                         PushStorePathFailed "foo" "daemon stopped",
                         PushFinished
                       ]

      it "keeps shared paths active for jobs that have not failed" $ do
        events <- newTVarIO []
        withPushManagerOnEvent (recordEvents events) $ \pm -> do
          (failedId, completedId) <- runPushManager pm $ do
            let request = Protocol.PushRequest {Protocol.storePaths = ["foo"], Protocol.subscribeToUpdates = True}
                pathSet = Set.singleton "foo"
                closure = PushJob.ResolvedClosure pathSet pathSet
            Just failedId <- addPushJobFromRequest request
            Just completedId <- addPushJobFromRequest request
            resolvePushJob failedId closure
            resolvePushJob completedId closure
            failPushJob failedId "daemon stopped"
            pushStorePathDone "foo"
            pure (failedId, completedId)

          recordedEvents <- reverse <$> readTVarIO events
          messagesFor failedId recordedEvents
            `shouldBe` [ PushStarted,
                         PushStorePathFailed "foo" "daemon stopped",
                         PushFinished
                       ]
          messagesFor completedId recordedEvents
            `shouldBe` [ PushStarted,
                         PushStorePathDone "foo",
                         PushFinished
                       ]

          failedJob <- runPushManager pm $ lookupPushJob failedId
          PushJob.status <$> failedJob `shouldBe` Just Failed
          completedJob <- runPushManager pm $ lookupPushJob completedId
          completedJob `shouldBe` Nothing
          runPushManager pm pendingJobCount `shouldReturn` 0

    describe "graceful shutdown" $ do
      it "shuts down with no jobs" $
        withPushManager $ \pm -> do
          _ <- drainPushManager timeoutOptions pm
          closePushManager pm

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
        Async.concurrently_ (drainPushManager longTimeoutOptions pm) $
          runPushManager pm $
            for_ paths pushStorePathDone
        endTime <- getCurrentTime

        let elapsed = diffUTCTime endTime startTime
        elapsed `shouldSatisfy` (< 0.5)
        closePushManager pm

      it "shuts down on job stall" $
        withPushManager $ \pm -> do
          _ <- runPushManager pm $ do
            let request = Protocol.PushRequest {Protocol.storePaths = ["foo"], Protocol.subscribeToUpdates = False}
            addPushJobFromRequest request

          _ <- drainPushManager timeoutOptions pm
          closePushManager pm

  describe "STM" $
    describe "timeout" $ do
      it "times out a transaction after n seconds" $ do
        timestamp <- newTVarIO =<< getCurrentTime
        result <- atomicallyWithTimeout timeoutOptions timestamp retry
        result `shouldBe` False

withPushManager :: (PushManagerEnv -> IO a) -> IO a
withPushManager = withPushManagerOnEvent mempty

withPushManagerOnEvent :: OnPushEvent -> (PushManagerEnv -> IO a) -> IO a
withPushManagerOnEvent onPushEvent f = do
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
    newPushManagerEnv pushOptions batchOptions pushParams onPushEvent logger >>= f

recordEvents :: TVar [PushEvent] -> OnPushEvent
recordEvents events _ event = atomically $ modifyTVar' events (event :)

assertStillRunning :: Async.Async () -> IO ()
assertStillRunning thread = do
  result <- Timeout.timeout 100000 $ Async.wait thread
  result `shouldBe` Nothing

isProgressEvent :: PushEvent -> Bool
isProgressEvent PushEvent {eventMessage = PushStorePathProgress {}} = True
isProgressEvent _ = False

messagesFor :: Protocol.PushRequestId -> [PushEvent] -> [PushEventMessage]
messagesFor pushId = map eventMessage . filter ((== pushId) . eventPushId)

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
