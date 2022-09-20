{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Activate where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.InstallationMode as InstallationMode
import qualified Cachix.Client.NetRc as NetRc
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Types.BinaryCache as BinaryCache
import Cachix.Types.Permission (Permission (..))
import qualified Data.Aeson as Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as HM
#else
import qualified Data.HashMap.Strict as HM
#endif
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.Process as Conduit
import qualified Data.Vector as Vector
import Protolude hiding (log, toS)
import Protolude.Conv (toS)
import Servant.Auth.Client (Token (..))
import qualified System.Directory as Directory
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Prelude (String)

data Status
  = Success
  | Failure FailureReason
  | Rollback FailureReason
  deriving (Show, Exception)

data FailureReason
  = NetworkTestFailure
  | RollbackScriptExitFailure
  | RollbackScriptUnexpectedError IOException
  | ShellCommandFailure {command :: String, exitCode :: Int}
  | UnexpectedError SomeException
  deriving (Show)

instance Exception FailureReason where
  displayException = \case
    NetworkTestFailure -> "Cannot connect back to Cachix Deploy after activating the new deployment"
    RollbackScriptExitFailure -> "The rollback script returned a non-zero exit code"
    RollbackScriptUnexpectedError e -> "Cannot run rollback script: " <> displayException e
    ShellCommandFailure {command, exitCode} ->
      toS $
        unwords
          [ "Failed to run " <> toS command,
            show exitCode
          ]
    UnexpectedError e ->
      toS $
        unwords
          [ "The deployment failed with an unexpected error:",
            toS (displayException e)
          ]

downloadStorePaths ::
  -- | Logging context
  Log.LogStream ->
  -- | Deployment details
  WSS.DeploymentDetails ->
  -- | Binary cache args
  [String] ->
  IO ()
downloadStorePaths logStream deploymentDetails cacheArgs = do
  -- Download the store path from the binary cache
  -- TODO: add GC root so it's preserved for the next command
  runShell logStream "nix-store" (["-r", toS storePath] <> cacheArgs)

  -- Download the rollback script, if provided
  for_ (WSS.rollbackScript deploymentDetails) $ \script ->
    runShell logStream "nix-store" (["-r", toS script] <> cacheArgs)
  where
    storePath = WSS.storePath deploymentDetails

type RollbackAction = IO ()

-- | Activate the profile and return a rollback action if available.
activate ::
  -- | Logging context
  Log.LogStream ->
  -- | Profile name
  Text ->
  -- | Store path to activate
  FilePath ->
  -- | Returns a rollback action if available
  IO (Maybe RollbackAction)
activate logStream profileName storePath = do
  (profilePath, activationScripts) <- getActivationScript profileName storePath
  previousProfilePath <- toStorePath profilePath

  -- Set the new profile
  -- TODO: document what happens if the wrong user is used for the agent
  runShell logStream "nix-env" ["-p", toS profilePath, "--set", toS storePath]

  -- Activate the configuration
  -- TODO: Check with Domen whether we can exit early here
  forM_ activationScripts $ uncurry (runShell logStream)

  pure $ Just rollback <*> previousProfilePath
  where
    toStorePath profilePath = do
      profileExists <- Directory.doesPathExist profilePath
      if profileExists
        then Just <$> Directory.canonicalizePath profilePath
        else pure Nothing

    -- We can't use '--rollback' because it just selects the next generation
    -- down from our deployment, which is not necessarily the generation that
    -- was previously active.
    rollback = void . activate logStream profileName

type Command = (String, [String])

-- TODO: home-manager
getActivationScript :: Text -> FilePath -> IO (FilePath, [Command])
getActivationScript profile storePath = do
  isNixOS <- Directory.doesPathExist $ toS storePath </> "nixos-version"
  isNixDarwin <- Directory.doesPathExist $ toS storePath </> "darwin-version"
  void InstallationMode.getUser
  (systemProfile, cmds) <- case (isNixOS, isNixDarwin) of
    (True, _) -> return ("system", [(toS storePath </> "bin/switch-to-configuration", ["switch"])])
    (_, True) ->
      -- https://github.com/LnL7/nix-darwin/blob/master/pkgs/nix-tools/darwin-rebuild.sh
      return
        ( "system-profiles/system",
          [ ("mkdir", ["-p", "-m", "0755", "/nix/var/nix/profiles/system-profiles"]),
            (toS storePath </> "activate-user", []),
            (toS storePath </> "activate", [])
          ]
        )
    (_, _) -> return ("system", [])
  return ("/nix/var/nix/profiles" </> if profile == "" then systemProfile else toS profile, cmds)

-- TODO: send errors as well
-- TODO: fix either/maybe types
getClosureSize :: [String] -> Text -> IO (Either Text (Maybe Int64))
getClosureSize cacheArgs storePath = do
  (exitCode, pathInfoJSON, nixError) <- readProcessWithExitCode "nix" (cacheArgs <> ["--extra-experimental-features", "nix-command", "path-info", "-S", "--json", toS storePath]) ""
  case exitCode of
    ExitFailure _ -> pure $ Left (toS nixError)
    ExitSuccess -> pure $ Right $ Aeson.decode (toS pathInfoJSON) >>= extractClosureSize

extractClosureSize :: Aeson.Value -> Maybe Int64
extractClosureSize (Aeson.Array vector) = case Vector.toList vector of
  [Aeson.Object obj] -> case HM.lookup "closureSize" obj of
    Just (Aeson.Number num) -> Just $ floor num
    _ -> Nothing
  _ -> Nothing
extractClosureSize _ = Nothing

domain :: Text -> WSS.Cache -> Text
domain host cache = toS (WSS.cacheName cache) <> "." <> toS host

-- TODO: get uri scheme
uri :: Text -> WSS.Cache -> Text
uri host cache = "https://" <> domain host cache

-- TODO: don't create tmpfile for public caches
withCacheArgs :: Text -> WSS.AgentInformation -> Text -> ([String] -> IO a) -> IO a
withCacheArgs host agentInfo agentToken m =
  withSystemTempDirectory "netrc" $ \dir -> do
    let filepath = dir </> "netrc"
    args <- case WSS.cache agentInfo of
      Just cache -> do
        -- TODO: ugh
        let bc =
              BinaryCache.BinaryCache
                { BinaryCache.name = "",
                  BinaryCache.uri = toS (uri host cache),
                  BinaryCache.publicSigningKeys = [],
                  BinaryCache.isPublic = WSS.isPublic cache,
                  BinaryCache.githubUsername = "",
                  BinaryCache.permission = Read
                }
        NetRc.add (Token (toS agentToken)) [bc] filepath
        return $ cachesArgs <> ["--option", "netrc-file", filepath]
      Nothing ->
        return cachesArgs
    m args
  where
    cachesArgs :: [String]
    cachesArgs = case WSS.cache agentInfo of
      Just cache ->
        let officialCache = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            substituters = ["--option", "extra-substituters", toS (uri host cache)]
            noNegativeCaching = ["--option", "narinfo-cache-negative-ttl", "0"]
            sigs = ["--option", "trusted-public-keys", officialCache <> " " <> toS (domain host cache) <> "-1:" <> toS (WSS.publicKey cache)]
         in substituters ++ sigs ++ noNegativeCaching
      Nothing -> []

runShell :: Log.LogStream -> FilePath -> [String] -> IO ()
runShell logStream cmd args = runShellWithExitCode logStream cmd args >>= handleError
  where
    handleError (ExitFailure exitCode) = throwIO $ ShellCommandFailure {command = cmd, exitCode}
    handleError ExitSuccess = pure ()

runShellWithExitCode :: Log.LogStream -> FilePath -> [String] -> IO ExitCode
runShellWithExitCode logStream cmd args = do
  Log.streamLine logStream $ "$ " <> toS cmd <> " " <> toS (unwords $ fmap toS args)
  (exitCode, _, _) <- Conduit.sourceProcessWithStreams (proc cmd args) Conduit.sinkNull logStream logStream
  pure exitCode
