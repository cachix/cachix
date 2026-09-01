module DeploySpec where

import Cachix.API.WebSocketSubprotocol qualified as DeploymentDetails (DeploymentDetails (..))
import Cachix.API.WebSocketSubprotocol qualified as WSS
import Cachix.Client.Config qualified as Config
import Cachix.Deploy.Agent (Agent (..), launchDeploymentWith, mkAgent, registerAgent, waitForAgent)
import Cachix.Deploy.Lock (withTryLock, withTryLockAndPid)
import Cachix.Deploy.Log qualified as Log
import Cachix.Deploy.OptionsParser qualified as CLI
import Cachix.Deploy.Websocket qualified as WebSocket
import Control.Concurrent.MVar qualified as MVar
import Control.Retry qualified as Retry
import Data.IORef
import Data.UUID qualified as UUID
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

    describe "WebSocket connection timeout" $ do
      it "interrupts stalled connection establishment" $ do
        let stalledConnection = do
              void (MVar.newEmptyMVar >>= MVar.takeMVar :: IO ())
              pure ((), pure ())
        WebSocket.withConnectionTimeout (10 * 1000) stalledConnection pure
          `shouldThrow` (== WebSocket.WebSocketConnectionTimeout)

      it "closes an established connection after the client exits" $ do
        closed <- MVar.newEmptyMVar
        WebSocket.withConnectionTimeout (10 * 1000) (pure ((), MVar.putMVar closed ())) pure
        MVar.tryTakeMVar closed `shouldReturn` Just ()

    describe "deployment commands" $ do
      it "runs a deployment ID only once" $
        withSystemTempDirectory "cachix-deploy-test" $ \tempDir ->
          withTestAgent tempDir $ \agent -> do
            registerTestAgent agent
            launches <- newIORef (0 :: Int)
            let runDeployment _ = modifyIORef' launches (+ 1) $> ExitSuccess
                deployment = testDeployment UUID.nil

            launchDeploymentWith runDeployment agent deployment
            launchDeploymentWith runDeployment agent deployment

            readIORef launches `shouldReturn` 1

      it "runs a new deployment ID" $
        withSystemTempDirectory "cachix-deploy-test" $ \tempDir ->
          withTestAgent tempDir $ \agent -> do
            registerTestAgent agent
            launches <- newIORef (0 :: Int)
            let runDeployment _ = modifyIORef' launches (+ 1) $> ExitSuccess

            launchDeploymentWith runDeployment agent (testDeployment UUID.nil)
            launchDeploymentWith runDeployment agent (testDeployment (UUID.fromWords 0 0 0 1))

            readIORef launches `shouldReturn` 2

      it "ignores an older deployment ID after a newer deployment" $
        withSystemTempDirectory "cachix-deploy-test" $ \tempDir ->
          withTestAgent tempDir $ \agent -> do
            registerTestAgent agent
            launches <- newIORef (0 :: Int)
            let runDeployment _ = modifyIORef' launches (+ 1) $> ExitSuccess
                firstDeployment = testDeployment UUID.nil
                secondDeployment = testDeployment (UUID.fromWords 0 0 0 1)

            launchDeploymentWith runDeployment agent firstDeployment
            launchDeploymentWith runDeployment agent secondDeployment
            launchDeploymentWith runDeployment agent firstDeployment

            readIORef launches `shouldReturn` 2

      it "retries a deployment after the child process fails" $
        withSystemTempDirectory "cachix-deploy-test" $ \tempDir ->
          withTestAgent tempDir $ \agent -> do
            registerTestAgent agent
            results <- newIORef [ExitFailure 1, ExitSuccess]
            let runDeployment _ = atomicModifyIORef' results $ \case
                  result : remaining -> (remaining, result)
                  [] -> ([], ExitSuccess)
                deployment = testDeployment UUID.nil

            launchDeploymentWith runDeployment agent deployment
            launchDeploymentWith runDeployment agent deployment

            readIORef results `shouldReturn` []

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

registerTestAgent :: Agent -> IO ()
registerTestAgent agent =
  registerAgent agent WSS.AgentInformation {WSS.cache = Nothing, WSS.id = UUID.nil}

testDeployment :: UUID.UUID -> WSS.DeploymentDetails
testDeployment deploymentID =
  DeploymentDetails.DeploymentDetails
    { DeploymentDetails.storePath = "/nix/store/test",
      DeploymentDetails.id = deploymentID,
      DeploymentDetails.index = 1,
      DeploymentDetails.rollbackScript = Nothing
    }
