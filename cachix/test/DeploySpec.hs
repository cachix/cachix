module DeploySpec where

import qualified Cachix.Client.Config as Config
import Cachix.Deploy.Agent (Agent (..), mkAgent, waitForAgent)
import Cachix.Deploy.Lock (withTryLock, withTryLockAndPid)
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Deploy.OptionsParser as CLI
import qualified Control.Retry as Retry
import Protolude
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec =
  describe "deploy" $ do
    describe "lock" $ do
      it "returns Nothing if the lock is free" $
        withSystemTempDirectory "cachix-deploy-test" $ \tempDir ->
          withTestAgent tempDir $ \agent -> do
            mpid <- waitForAgent retryPolicy agent
            mpid `shouldBe` Nothing

      it "returns Nothing if there's no PID" $
        withSystemTempDirectory "cachix-deploy-test" $ \tempDir ->
          withTestAgent tempDir $ \agent -> do
            void $ withTryLock (lockFile agent) $ do
              mpid <- waitForAgent retryPolicy agent
              mpid `shouldBe` Nothing

      it "returns the PID if the lock is taken" $
        withSystemTempDirectory "cachix-deploy-test" $ \tempDir ->
          withTestAgent tempDir $ \agent -> do
            void $ withTryLockAndPid (lockFile agent) (pidFile agent) $ do
              mpid <- waitForAgent retryPolicy agent
              mpid `shouldSatisfy` isJust

withTestAgent :: FilePath -> (Agent -> IO ()) -> IO ()
withTestAgent tempDir action = do
  let logOptions =
        Log.Options
          { verbosity = Log.Verbose,
            namespace = "agent",
            environment = "Test"
          }
      agentOptions =
        CLI.AgentOptions
          { name = "foo",
            profile = Just "testing",
            bootstrap = False
          }
      agentToken = ""

  cachixOptions <- Config.defaultCachixOptions

  Log.withLog logOptions $ \withLog -> do
    agent <- mkAgent withLog logOptions (Just tempDir) cachixOptions agentOptions agentToken
    action agent

retryPolicy :: Retry.RetryPolicyM IO
retryPolicy = Retry.limitRetries 1 <> Retry.constantDelay 10
