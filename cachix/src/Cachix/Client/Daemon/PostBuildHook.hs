{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cachix.Client.Daemon.PostBuildHook where

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
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files

withSetup :: Maybe FilePath -> (FilePath -> Text -> IO a) -> IO a
withSetup mdaemonSock f =
  withSystemTempDirectory "cachix-daemon" $ \tempDir -> do
    let postBuildHookScriptPath = tempDir </> "post-build-hook.sh"
        postBuildHookConfigPath = tempDir </> "nix.conf"
        daemonSock = fromMaybe (tempDir </> "daemon.sock") mdaemonSock

    cachixBin <- getExecutablePath

    writeFile postBuildHookScriptPath (postBuildHookScript cachixBin daemonSock)
    setFileMode postBuildHookScriptPath 0o755
    writeFile postBuildHookConfigPath (postBuildHookConfig postBuildHookScriptPath)

    nixUserConfFilesEnv <- buildNixUserConfFilesEnv postBuildHookConfigPath

    f daemonSock nixUserConfFilesEnv

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
buildNixUserConfFilesEnv :: FilePath -> IO Text
buildNixUserConfFilesEnv nixConfPath = do
  -- A user can set NIX_USER_CONF_FILES to override the default nix.conf files.
  -- In that case, we reuse it and prepend our own config file.
  mexistingEnv <- lookupEnv "NIX_USER_CONF_FILES"

  case mexistingEnv of
    Just existingEnv -> return $ toS $ nixConfPath <> ":" <> existingEnv
    Nothing -> do
      userConfigFiles <- getUserConfigFiles

      -- Combine all the nix.conf paths into one string, separated by colons.
      -- Filter out empty paths.
      return $ toS $ intercalate ":" $ filter (not . null) $ nixConfPath : userConfigFiles

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
export IFS=''

exec ${toS cachixBin :: Text} daemon push \\
  --socket ${toS socketPath :: Text} \\
  $OUT_PATHS
  |]
