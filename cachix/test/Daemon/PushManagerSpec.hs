module Daemon.PushManagerSpec where

import qualified Cachix.Client.Daemon.Log as Log
import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.PushManager
import qualified Cachix.Client.Daemon.PushManager.PushJob as PushJob
import Cachix.Client.Daemon.Types.PushManager
import Cachix.Client.OptionsParser (defaultPushOptions)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM.TVar
import Control.Monad (fail)
import Control.Retry (defaultRetryStatus)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Protolude
import Test.Hspec

instance MonadFail PushManager where
  fail msg = liftIO (expectationFailure msg) >> mzero

spec :: Spec
spec = do
  describe "push job" $ do
    it "starts in the queued state" $ do
      let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"]}
      pushJob <- PushJob.new request
      PushJob.status pushJob `shouldBe` Queued

    it "can be resolved" $ do
      let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"]}
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
        let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"]}
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
        let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"]}
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

  describe "push manager" $ do
    it "queues push jobs " $ inPushManager $ do
      let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"]}
      Just pushId <- addPushJob request
      Just pushJob <- lookupPushJob pushId
      liftIO $ do
        PushJob.pushId pushJob `shouldBe` pushId
        PushJob.pushRequest pushJob `shouldBe` request

    it "manages the lifecycle of a push job" $ inPushManager $ do
      let paths = ["bar", "foo"]

      let pushRequest = Protocol.PushRequest {Protocol.storePaths = paths}
      Just pushId <- addPushJob pushRequest

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

    describe "graceful shutdown" $ do
      it "shuts down with no jobs" $ do
        logger <- Log.new "daemon" Nothing Log.Debug
        pm <- newPushManagerEnv defaultPushOptions logger mempty
        stopPushManager timeoutOptions pm

      it "shuts down after jobs complete" $ do
        let paths = ["foo"]
        let longTimeoutOptions = TimeoutOptions {toTimeout = 10.0, toPollingInterval = 0.1}
        logger <- Log.new "daemon" Nothing Log.Debug
        pm <- newPushManagerEnv defaultPushOptions logger mempty

        _ <- runPushManager pm $ do
          let request = Protocol.PushRequest {Protocol.storePaths = paths}
          addPushJob request

        concurrently_ (stopPushManager longTimeoutOptions pm) $
          runPushManager pm $
            for_ paths pushStorePathDone

      it "shuts down on job stall" $ do
        logger <- Log.new "daemon" Nothing Log.Debug
        pm <- newPushManagerEnv defaultPushOptions logger mempty
        _ <- runPushManager pm $ do
          let request = Protocol.PushRequest {Protocol.storePaths = ["foo"]}
          addPushJob request

        stopPushManager timeoutOptions pm

  describe "STM" $
    describe "timeout" $ do
      it "times out a transaction after n seconds" $ do
        timestamp <- newTVarIO =<< getCurrentTime
        atomicallyWithTimeout timeoutOptions timestamp retry

withPushManager :: (PushManagerEnv -> IO a) -> IO a
withPushManager f = do
  logger <- liftIO $ Log.new "daemon" Nothing Log.Debug
  newPushManagerEnv defaultPushOptions logger mempty >>= f

inPushManager :: PushManager a -> IO a
inPushManager f = withPushManager (`runPushManager` f)

timeoutOptions :: TimeoutOptions
timeoutOptions = TimeoutOptions {toTimeout = 0.2, toPollingInterval = 0.1}
