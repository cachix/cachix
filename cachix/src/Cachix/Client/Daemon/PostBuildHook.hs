{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cachix.Client.Daemon.PostBuildHook where

import Data.String.Here
import Protolude
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
    nixUserConfFiles <- lookupEnv "NIX_USER_CONF_FILES"
    let newNixUserConfFiles = foldMap identity $ intersperse ":" $ catMaybes [nixUserConfFiles, Just postBuildHookConfigPath]

    writeFile postBuildHookScriptPath (postBuildHookScript cachixBin daemonSock)
    setFileMode postBuildHookScriptPath 0o755
    writeFile postBuildHookConfigPath (postBuildHookConfig postBuildHookScriptPath)

    f daemonSock (toS newNixUserConfFiles)

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
