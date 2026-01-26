module Daemon.PostBuildHookSpec where

import Cachix.Daemon.PostBuildHook
import Data.String
import Protolude
import System.Environment qualified as System
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (getCanonicalTemporaryDirectory)
import Test.Hspec

spec :: Spec
spec = do
  let scriptPath = "build-hook.sh"
      configPath = "my-nix.conf"

  describe "withRunnerFriendlyTempDirectory" $ do
    it "uses RUNNER_TEMP when path fits within socket limit" $ do
      systemTempDir <- getCanonicalTemporaryDirectory
      -- Use a short path that will fit
      withTempEnv ("RUNNER_TEMP", systemTempDir) $ do
        withRunnerFriendlyTempDirectory "cachix-daemon" $ \tempDir -> do
          takeDirectory tempDir `shouldBe` systemTempDir

    it "falls back to system temp when RUNNER_TEMP would exceed socket path limit" $ do
      systemTempDir <- getCanonicalTemporaryDirectory
      -- Create a path long enough to exceed the limit when combined with daemon socket path
      -- unixSocketMaxPath is 104 (macOS/BSD) or 108 (Linux)
      -- We need: dir + "/cachix-daemonXXXXXX" (19) + "/daemon.sock" (12) >= limit
      -- So dir needs to be >= limit - 31 = 73 (macOS) or 77 (Linux)
      let longPath = "/tmp" </> replicate (unixSocketMaxPath - 20) 'x'
      withTempEnv ("RUNNER_TEMP", longPath) $ do
        withRunnerFriendlyTempDirectory "cachix-daemon" $ \tempDir -> do
          -- Should fall back to system temp, not use the long RUNNER_TEMP
          takeDirectory tempDir `shouldBe` systemTempDir

    it "uses system temp when RUNNER_TEMP is not set" $ do
      systemTempDir <- getCanonicalTemporaryDirectory
      withTempEnv ("RUNNER_TEMP", "") $ do
        System.unsetEnv "RUNNER_TEMP"
        withRunnerFriendlyTempDirectory "cachix-daemon" $ \tempDir -> do
          takeDirectory tempDir `shouldBe` systemTempDir

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
