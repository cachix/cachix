module Daemon.PostBuildHookSpec where

import Cachix.Daemon.PostBuildHook
import Data.String
import Protolude
import System.Environment qualified as System
import Test.Hspec

spec :: Spec
spec = do
  let scriptPath = "build-hook.sh"
      configPath = "my-nix.conf"

  describe "post build hook" $ do
    it "builds the NIX_CONF environment variable" $ do
      withTempEnv ("NIX_CONF", "") $ do
        buildNixConfEnv scriptPath `shouldReturn` Nothing

      withTempEnv ("NIX_CONF", "max-jobs = 8") $ do
        buildNixConfEnv scriptPath `shouldReturn` Just ("NIX_CONF", "max-jobs = 8\npost-build-hook = " <> scriptPath)

    it "builds the NIX_USER_CONF_FILES environment variable" $ do
      withTempEnv ("NIX_USER_CONF_FILES", "") $ do
        ("NIX_USER_CONF_FILES", conf) <- buildNixUserConfFilesEnv configPath
        -- Should contain /etc/xdg and home config dirs as well
        conf `shouldContain` configPath

      withTempEnv ("NIX_USER_CONF_FILES", "/some/nix.conf") $ do
        ("NIX_USER_CONF_FILES", conf) <- buildNixUserConfFilesEnv configPath
        conf `shouldBe` intercalate ":" [configPath, "/some/nix.conf"]

withTempEnv :: (String, String) -> IO a -> IO a
withTempEnv (envName, envValue) f = bracket setEnv unsetEnv (const f)
  where
    setEnv = do
      mprevEnv <- System.lookupEnv envName
      System.setEnv envName envValue
      return mprevEnv
    unsetEnv =
      maybe (System.unsetEnv envName) (System.setEnv envName)
