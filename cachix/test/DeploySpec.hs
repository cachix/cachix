module DeploySpec where

import Cachix.Deploy.Agent (lockFilename, waitForAgent)
import Cachix.Deploy.Lock (withTryLock, withTryLockAndPid)
import qualified Control.Retry as Retry
import Protolude
import Test.Hspec

spec :: Spec
spec =
  describe "deploy" $ do
    describe "lock" $ do
      it "returns Nothing if the lock is free" $ do
        mpid <- waitForAgent "foo" retryPolicy
        mpid `shouldBe` Nothing

      it "returns Nothing if there's no PID" $ do
        void $ withTryLock (lockFilename "foo") $ do
          mpid <- waitForAgent "foo" retryPolicy
          mpid `shouldBe` Nothing

      it "returns the PID if the lock is taken" $ do
        void $ withTryLockAndPid (lockFilename "foo") $ do
          mpid <- waitForAgent "foo" retryPolicy
          mpid `shouldSatisfy` isJust

retryPolicy :: Retry.RetryPolicyM IO
retryPolicy = Retry.limitRetries 1 <> Retry.constantDelay 10
