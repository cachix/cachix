module Daemon.PushManagerSpec where

import qualified Cachix.Client.Daemon.Log as Log
import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.PushManager
import Cachix.Client.Daemon.Types.PushManager
import Control.Retry (defaultRetryStatus)
import qualified Data.Set as Set
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "push manager" $ do
    it "queues push jobs " $ withPushManager $ \pushManager -> do
      let request = Protocol.PushRequest {Protocol.storePaths = ["foo", "bar"]}
      Just pushJob <- runPushManager pushManager $ do
        pushId <- addPushJob request
        lookupPushJob pushId
      pushRequest pushJob `shouldBe` request

    it "manages the lifecycle of a push job" $ withPushManager $ \pushManager -> do
      let paths = ["bar", "foo"]

      pushId <- runPushManager pushManager $ do
        let pushRequest = Protocol.PushRequest {Protocol.storePaths = paths}
        pushId <- addPushJob pushRequest
        resolvePushJob pushId paths paths
        return pushId

      do
        Just pushJob <- runPushManager pushManager $ do
          pushStarted pushId
          lookupPushJob pushId
        pushStartedAt pushJob `shouldSatisfy` isJust

      Just pushJob <- runPushManager pushManager $ do
        forM_ paths $ \path -> do
          pushStorePathAttempt path 1 defaultRetryStatus
          pushStorePathDone path
        lookupPushJob pushId

      pushDetails pushJob
        `shouldBe` PushDetails
          { pdAllPaths = Set.fromList paths,
            pdQueuedPaths = mempty,
            pdFailedPaths = mempty,
            pdPushedPaths = Set.fromList paths,
            pdSkippedPaths = mempty
          }

      pushFinishedAt pushJob `shouldSatisfy` isJust

withPushManager :: (PushManagerEnv -> IO ()) -> IO ()
withPushManager f = do
  logger <- liftIO $ Log.new "daemon" Nothing Log.Debug
  newPushManagerEnv logger mempty >>= f
