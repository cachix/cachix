{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Activate where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.InstallationMode as InstallationMode
import qualified Cachix.Client.NetRc as NetRc
import qualified Cachix.Deploy.Websocket as CachixWebsocket
import qualified Cachix.Types.BinaryCache as BinaryCache
import Cachix.Types.Permission (Permission (..))
import qualified Data.Aeson as Aeson
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as HM
#else
import qualified Data.HashMap.Strict as HM
#endif
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.Process as Conduit
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID.V4 as UUID
import qualified Data.Vector as Vector
import qualified Network.WebSockets as WS
import Protolude hiding (log, toS)
import Protolude.Conv (toS)
import Servant.Auth.Client (Token (..))
import System.Directory (canonicalizePath, doesPathExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Timeout (timeout)
import Prelude (String)

domain :: CachixWebsocket.Options -> WSS.Cache -> Text
domain options cache = toS (WSS.cacheName cache) <> "." <> toS (CachixWebsocket.host options)

-- TODO: get uri scheme
uri :: CachixWebsocket.Options -> WSS.Cache -> Text
uri options cache = "https://" <> domain options cache

-- TODO: what if websocket gets closed while deploying?
activate ::
  CachixWebsocket.Options ->
  WS.Connection -> -- REMOVE
  Conduit.ConduitT ByteString Void IO () ->
  WSS.DeploymentDetails ->
  WSS.AgentInformation ->
  ByteString ->
  IO ()
activate options connection logStream deploymentDetails agentInfo agentToken =
  withCacheArgs options agentInfo agentToken $ \cacheArgs -> do
    (_, pathInfoJSON, _) <- shellNoStream "nix" (cacheArgs <> ["--extra-experimental-features", "nix-command", "path-info", "-S", "--json", toS storePath])

    -- Notify the service that the deployment started
    getCurrentTime >>= \now ->
      sendMessage $
        WSS.DeploymentStarted
          { WSS.id = deploymentID,
            WSS.time = now,
            WSS.closureSize = extractClosureSize =<< Aeson.decode (toS pathInfoJSON)
          }

    -- TODO: add GC root so it's preserved for the next command
    -- Get the store path using caches
    (downloadExitCode, _, _) <-
      shellOut "nix-store" (["-r", toS storePath] <> cacheArgs)

    (profile, activationScripts) <- getActivationScript storePath (CachixWebsocket.profile options)

    -- Set the new profile
    -- TODO: document what happens if the wrong user is used for the agent
    (activateProfileExitCode, _, _) <- shellOut "nix-env" ["-p", toS profile, "--set", toS storePath]

    -- Activate the configuration
    exitCodes <- for activationScripts $ \(cmd, args) -> do
      (activateScriptExitCode, _, _) <- shellOut cmd args
      return activateScriptExitCode

    if not (all (== ExitSuccess) exitCodes)
      then deploymentFailed
      else do
        -- K.logLocM K.InfoS $ K.ls $ "Deployment #" <> index <> " finished"
        now <- getCurrentTime
        sendMessage $ deploymentFinished True now
        log "Successfully activated the deployment."
  where
    -- TODO: use exceptions to simplify this code
    -- case downloadExitCode of
    --   ExitFailure _ -> deploymentFailed
    --   ExitSuccess -> do
    --     (profile, activationScripts) <- getActivationScript storePath (CachixWebsocket.profile options)
    --     -- TODO: document what happens if wrong user is used for the agent

    --     -- set the new profile
    --     (activateProfileExitCode, _, _) <- shellOut "nix-env" ["-p", toS profile, "--set", toS storePath]
    --     case activateProfileExitCode of
    --       ExitFailure _ -> deploymentFailed
    --       ExitSuccess -> do
    --         -- activate configuration
    --         exitCodes <- for activationScripts $ \(cmd, args) -> do
    --           (activateScriptExitCode, _, _) <- shellOut cmd args
    --           return activateScriptExitCode
    --         if not (all (== ExitSuccess) exitCodes)
    --           then deploymentFailed
    --           else do
    --             -- K.logLocM K.InfoS $ K.ls $ "Deployment #" <> index <> " finished"
    --             now <- getCurrentTime
    --             sendMessage $ deploymentFinished True now
    --             log "Successfully activated the deployment."

    storePath = WSS.storePath deploymentDetails
    deploymentID = WSS.id (deploymentDetails :: WSS.DeploymentDetails)

    -- Move to Main.hs
    deploymentFinished hasSucceeded now =
      WSS.DeploymentFinished
        { WSS.id = deploymentID,
          WSS.time = now,
          WSS.hasSucceeded = hasSucceeded
        }

    -- Move to Main.hs
    deploymentFailed = do
      -- K.logLocM K.InfoS $ K.ls $ "Deploying #" <> index <> " failed."
      now <- getCurrentTime
      sendMessage $ deploymentFinished False now

    -- TODO: it would be better to stream and also return the text
    shellNoStream :: FilePath -> [String] -> IO (ExitCode, String, String)
    shellNoStream cmd args = do
      log $ "$ " <> toS cmd <> " " <> toS (unwords $ fmap toS args)
      (exitCode, procstdout, procstderr) <- readProcessWithExitCode cmd args ""
      log $ toS procstderr
      return (exitCode, toS procstdout, toS procstderr)

    shellOut :: FilePath -> [String] -> IO (ExitCode, (), ())
    shellOut cmd args = do
      log $ "$ " <> toS cmd <> " " <> toS (unwords $ fmap toS args)
      Conduit.sourceProcessWithStreams (proc cmd args) Conduit.sinkNull logStream logStream

    -- Move to Main.hs
    log :: ByteString -> IO ()
    log msg = Conduit.connect (Conduit.yieldMany ["\n" <> msg <> "\n"]) logStream

    -- Move to WebSocket
    sendMessage cmd = do
      command <- createMessage cmd
      WSS.sendMessage connection command

    createMessage :: WSS.AgentCommand -> IO (WSS.Message WSS.AgentCommand)
    createMessage command = do
      uuid <- UUID.nextRandom
      return $
        WSS.Message
          { WSS.method = method,
            WSS.command = command,
            WSS.id = uuid,
            WSS.agent = Just $ WSS.id (agentInfo :: WSS.AgentInformation)
          }
      where
        -- TODO: move to WSS
        method = case command of
          WSS.DeploymentStarted {} -> "DeploymentStarted"
          WSS.DeploymentFinished {} -> "DeploymentFinished"

    -- TODO: home-manager
    getActivationScript :: Text -> Text -> IO (FilePath, [Command])
    getActivationScript storePath profile = do
      isNixOS <- doesPathExist $ toS storePath </> "nixos-version"
      isNixDarwin <- doesPathExist $ toS storePath </> "darwin-version"
      user <- InstallationMode.getUser
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

type Command = (String, [String])

extractClosureSize :: Aeson.Value -> Maybe Int64
extractClosureSize (Aeson.Array vector) = case Vector.toList vector of
  [Aeson.Object obj] -> case HM.lookup "closureSize" obj of
    Just (Aeson.Number num) -> Just $ floor num
    _ -> Nothing
  _ -> Nothing
extractClosureSize _ = Nothing

-- TODO: don't create tmpfile for public caches
withCacheArgs :: CachixWebsocket.Options -> WSS.AgentInformation -> ByteString -> ([String] -> IO ()) -> IO ()
withCacheArgs options agentInfo agentToken m =
  withSystemTempDirectory "netrc" $ \dir -> do
    let filepath = dir </> "netrc"
    args <- case WSS.cache agentInfo of
      Just cache -> do
        -- TODO: ugh
        let bc =
              BinaryCache.BinaryCache
                { BinaryCache.name = "",
                  BinaryCache.uri = toS (uri options cache),
                  BinaryCache.publicSigningKeys = [],
                  BinaryCache.isPublic = WSS.isPublic cache,
                  BinaryCache.githubUsername = "",
                  BinaryCache.permission = Read
                }
        NetRc.add (Token agentToken) [bc] filepath
        return $ cachesArgs <> ["--option", "netrc-file", filepath]
      Nothing ->
        return cachesArgs
    m args
  where
    cachesArgs :: [String]
    cachesArgs = case WSS.cache agentInfo of
      Just cache ->
        let officialCache = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
            substituters = ["--option", "extra-substituters", toS (uri options cache)]
            noNegativeCaching = ["--option", "narinfo-cache-negative-ttl", "0"]
            sigs = ["--option", "trusted-public-keys", officialCache <> " " <> toS (domain options cache) <> "-1:" <> toS (WSS.publicKey cache)]
         in substituters ++ sigs ++ noNegativeCaching
      Nothing -> []
