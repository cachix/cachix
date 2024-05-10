{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cachix.Client.Daemon.PostBuildHook where

import Control.Monad.Catch (MonadMask)
import Data.Containers.ListUtils (nubOrd)
import Data.String (String)
import Data.String.Here
import Protolude
import System.Directory
  ( XdgDirectory (XdgConfig),
    XdgDirectoryList (XdgConfigDirs),
    getXdgDirectory,
    getXdgDirectoryList,
  )
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (getCanonicalTemporaryDirectory, withTempDirectory)
import System.Posix.Files

type EnvVar = (String, String)

modifyEnv :: EnvVar -> [EnvVar] -> [EnvVar]
modifyEnv (envName, envValue) processEnv =
  nubOrd $ (envName, envValue) : processEnv

withSetup :: Maybe FilePath -> (FilePath -> EnvVar -> IO a) -> IO a
withSetup mdaemonSock f =
  withDaemonTempDirectory "cachix-daemon" $ \tempDir -> do
    let postBuildHookScriptPath = tempDir </> "post-build-hook.sh"
        postBuildHookConfigPath = tempDir </> "nix.conf"
        daemonSock = fromMaybe (tempDir </> "daemon.sock") mdaemonSock

    cachixBin <- getExecutablePath
    writeFile postBuildHookScriptPath (postBuildHookScript cachixBin daemonSock)
    setFileMode postBuildHookScriptPath 0o755

    mnixConfEnv <- buildNixConfEnv postBuildHookScriptPath
    nixUserConfFilesEnv <- buildNixUserConfFilesEnv postBuildHookConfigPath
    nixConfEnvVar <- case mnixConfEnv of
      Just nixConfEnv -> return nixConfEnv
      Nothing -> do
        writeFile postBuildHookConfigPath (postBuildHookConfig postBuildHookScriptPath)
        return nixUserConfFilesEnv

    f daemonSock nixConfEnvVar

-- | Build the NIX_CONF environment variable.
--
-- NIX_CONF completely overrides the nix.conf.
-- This is generally undesirable because the user and system nix.confs contain important settings, like substituters.
-- Therefore, this returns Nothing if NIX_CONF is not already set to allow fallback to NIX_USER_CONF_FILES.
buildNixConfEnv :: FilePath -> IO (Maybe EnvVar)
buildNixConfEnv postBuildHookScriptPath =
  fmap appendNixConf <$> lookupEnv "NIX_CONF"
  where
    appendNixConf :: String -> EnvVar
    appendNixConf conf =
      ( "NIX_CONF",
        conf <> "\n" <> toS (postBuildHookConfig postBuildHookScriptPath)
      )

-- | Build the NIX_USER_CONF_FILES environment variable.
--
-- From man nix.conf:
--
-- If NIX_USER_CONF_FILES is set, then each path separated by : will be loaded in reverse order.
--
-- Otherwise it will look for nix/nix.conf files in XDG_CONFIG_DIRS and XDG_CONFIG_HOME. If
-- unset, XDG_CONFIG_DIRS defaults to /etc/xdg, and XDG_CONFIG_HOME defaults to $HOME/.config
-- as per XDG Base Directory Specification.
--
-- We don't need to load the system config from $NIX_CONF_DIR/nix.conf.
-- Nix loads it by default and uses it as the base config.
buildNixUserConfFilesEnv :: FilePath -> IO EnvVar
buildNixUserConfFilesEnv nixConfPath = do
  -- A user can set NIX_USER_CONF_FILES to override the default nix.conf files.
  -- In that case, we reuse it and prepend our own config file.
  mexistingEnv <- lookupEnv "NIX_USER_CONF_FILES"

  newNixUserConfFiles <- case mexistingEnv of
    Just existingEnv -> return $ nixConfPath <> ":" <> existingEnv
    Nothing -> do
      userConfigFiles <- getUserConfigFiles

      -- Combine all the nix.conf paths into one string, separated by colons.
      -- Filter out empty paths.
      return $ intercalate ":" $ filter (not . null) $ nixConfPath : userConfigFiles

  return ("NIX_USER_CONF_FILES", newNixUserConfFiles)

getUserConfigFiles :: IO [FilePath]
getUserConfigFiles =
  fmap (</> "nix/nix.conf") <$> getUserConfigDirs

getUserConfigDirs :: IO [FilePath]
getUserConfigDirs = do
  configHome <- getXdgDirectory XdgConfig empty
  configDirs <- getXdgDirectoryList XdgConfigDirs

  return $ configHome : configDirs

postBuildHookConfig :: FilePath -> Text
postBuildHookConfig scriptPath =
  [iTrim|
post-build-hook = ${toS scriptPath :: Text}
  |]

postBuildHookScript :: FilePath -> FilePath -> Text
postBuildHookScript cachixBin socketPath =
  [iTrim|
\#!/bin/sh

\# set -eu
set -f # disable globbing

exec ${toS cachixBin :: Text} daemon push \\
  --socket ${toS socketPath :: Text} \\
  $OUT_PATHS
  |]

-- | Run an action with a temporary directory.
--
-- Respects the RUNNER_TEMP environment variable if set.
-- This is important on self-hosted GitHub runners with locked down system temp directories.
-- The directory is deleted after use.
withDaemonTempDirectory :: (MonadIO m, MonadMask m) => String -> (FilePath -> m a) -> m a
withDaemonTempDirectory name action = do
  runnerTempDir <- liftIO $ lookupEnv "RUNNER_TEMP"
  systemTempDir <- liftIO getCanonicalTemporaryDirectory
  let tempDir = maybe systemTempDir toS runnerTempDir
  withTempDirectory tempDir name action
