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
import qualified Katip as K
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

hackFlush :: K.KatipContextT IO ()
hackFlush = liftIO $ threadDelay (5 * 1000 * 1000)

-- TODO: what if websocket gets closed while deploying?
activate ::
  CachixWebsocket.Options ->
  WS.Connection ->
  Conduit.ConduitT ByteString Void IO () ->
  WSS.DeploymentDetails ->
  WSS.AgentInformation ->
  ByteString ->
  K.KatipContextT IO ()
activate options connection sourceStream deploymentDetails agentInfo agentToken = withCacheArgs options agentInfo agentToken $ \cacheArgs -> do
  let storePath = WSS.storePath deploymentDetails
  K.logLocM K.InfoS $ K.ls $ "Deploying #" <> index <> ": " <> WSS.storePath deploymentDetails
  deploymentStarted Nothing
  -- TODO: add GC root so it's preserved for the next command
  -- get the store path using caches
  (downloadExitCode, _, _) <-
    liftIO $ shellOut "nix-store" (["-r", toS storePath] <> cacheArgs)
  -- TODO: use exceptions to simplify this code
  case downloadExitCode of
    ExitFailure _ -> deploymentFailed
    ExitSuccess -> do
      -- TODO: query the remote store to get the size before downloading (and possibly running out of disk space)
      (_, pathInfoJSON, _) <- liftIO $ shellNoStream "nix" (cacheArgs <> ["--extra-experimental-features", "nix-command", "path-info", "-S", "--json", toS storePath])
      deploymentStarted $ extractClosureSize =<< Aeson.decode (toS pathInfoJSON)

      activateProfile storePath $ \oldStorePath -> do
        -- connect the the backend to see if we broke networking
        websocketSucceeded <- liftIO $ timeout (10 * 1000 * 1000) $ CachixWebsocket.runOnce options $ \_ _ _ _ -> return ()
        case websocketSucceeded of
          Nothing -> rollback oldStorePath
          Just () -> do
            case WSS.rollbackScript deploymentDetails of
              Just script -> do
                (downloadRollbackExitCode, _, _) <- liftIO $ shellOut "nix-store" (["-r", toS script] <> cacheArgs)
                case downloadRollbackExitCode of
                  ExitFailure _ -> deploymentFailed
                  ExitSuccess -> do
                    (rollbackExitCode, _, _) <- liftIO $ shellOut (toS script) []
                    case rollbackExitCode of
                      ExitFailure _ -> rollback oldStorePath
                      ExitSuccess -> deloymentSucceeded
              Nothing -> deloymentSucceeded
  where
    deploymentID = WSS.id (deploymentDetails :: WSS.DeploymentDetails)
    deloymentSucceeded = do
      now <- liftIO getCurrentTime
      sendMessage $ deploymentFinished True now
      liftIO $ log "Successfully activated the deployment."
      K.logLocM K.InfoS $ K.ls $ "Deployment #" <> index <> " finished"
      hackFlush
    deploymentStarted closureSize = do
      now <- liftIO getCurrentTime
      sendMessage $
        WSS.DeploymentStarted
          { WSS.id = deploymentID,
            WSS.time = now,
            WSS.closureSize = closureSize
          }
    deploymentFinished hasSucceeded now =
      WSS.DeploymentFinished
        { WSS.id = deploymentID,
          WSS.time = now,
          WSS.hasSucceeded = hasSucceeded
        }
    deploymentFailed = do
      now <- liftIO getCurrentTime
      liftIO $ log "Failed to activate the deployment."
      K.logLocM K.InfoS $ K.ls $ "Deploying #" <> index <> " failed."
      sendMessage $ deploymentFinished False now
      hackFlush
    rollback :: Maybe Text -> K.KatipContextT IO ()
    rollback (Just path) = do
      liftIO $ log "Deployment failed, rolling back ..."
      activateProfile path $ const deploymentFailed
    rollback Nothing = do
      liftIO $ log "Skipping rollback as this is the first deployment."
      deploymentFailed
    activateProfile :: Text -> (Maybe Text -> K.KatipContextT IO ()) -> K.KatipContextT IO ()
    activateProfile path action = do
      (profilePath, activationScripts) <- liftIO $ getActivationScript path (CachixWebsocket.profile options)
      profileExists <- liftIO $ doesPathExist profilePath
      -- in case we're doing the first deployment the profile won't exist so we can't rollback
      previousStorePath <-
        if profileExists
          then do
            oldStorePath <- liftIO $ canonicalizePath profilePath
            return $ Just $ toS oldStorePath
          else return Nothing
      (activateProfileExitCode, _, _) <- liftIO $ shellOut "nix-env" ["-p", toS profilePath, "--set", toS path]
      case activateProfileExitCode of
        ExitFailure _ -> deploymentFailed
        ExitSuccess -> do
          -- activate configuration
          exitCodes <- for activationScripts $ \(cmd, args) -> do
            (activateScriptExitCode, _, _) <- liftIO $ shellOut cmd args
            return activateScriptExitCode
          if not (all (== ExitSuccess) exitCodes)
            then deploymentFailed
            else action previousStorePath
    index :: Text
    index = show $ WSS.index deploymentDetails
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
      Conduit.sourceProcessWithStreams (proc cmd args) Conduit.sinkNull sourceStream sourceStream
    log :: ByteString -> IO ()
    log msg = Conduit.connect (Conduit.yieldMany ["\n" <> msg <> "\n"]) sourceStream
    sendMessage cmd = liftIO $ do
      command <- createMessage cmd
      WSS.sendMessage connection command
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
withCacheArgs :: CachixWebsocket.Options -> WSS.AgentInformation -> ByteString -> ([String] -> K.KatipContextT IO ()) -> K.KatipContextT IO ()
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
        liftIO $ NetRc.add (Token agentToken) [bc] filepath
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
