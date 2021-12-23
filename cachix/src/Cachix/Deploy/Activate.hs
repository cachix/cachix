{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Activate where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.NetRc as NetRc
import qualified Cachix.Client.OptionsParser as CachixOptions
import Cachix.Client.URI (getBaseUrl)
import qualified Cachix.Deploy.OptionsParser as AgentOptions
import qualified Cachix.Types.BinaryCache as BinaryCache
import Cachix.Types.Permission (Permission (..))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.Process as Conduit
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID.V4 as UUID
import qualified Katip as K
import qualified Network.WebSockets as WS
import Protolude hiding (log, toS)
import Protolude.Conv (toS)
import Servant.Auth.Client (Token (..))
import qualified Servant.Client as Servant
import System.Directory (doesPathExist)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Prelude (String)

-- TODO: duplicated in Agent.hs
host :: CachixOptions.CachixOptions -> String
host cachixOptions = Servant.baseUrlHost $ getBaseUrl $ CachixOptions.host cachixOptions

domain :: CachixOptions.CachixOptions -> WSS.Cache -> Text
domain cachixOptions cache = toS (WSS.cacheName cache) <> "." <> toS (host cachixOptions)

-- TODO: get uri scheme
uri :: CachixOptions.CachixOptions -> WSS.Cache -> Text
uri cachixOptions cache = "https://" <> domain cachixOptions cache

-- TODO: what if websocket gets closed while deploying?
activate ::
  CachixOptions.CachixOptions ->
  AgentOptions.AgentOptions ->
  WS.Connection ->
  Conduit.ConduitT ByteString Void IO () ->
  WSS.DeploymentDetails ->
  WSS.AgentInformation ->
  ByteString ->
  K.KatipContextT IO ()
activate cachixOptions agentArgs connection sourceStream deploymentDetails agentInfo agentToken = do
  let storePath = WSS.storePath deploymentDetails
      cachesArgs :: [String]
      cachesArgs = case WSS.cache agentInfo of
        Just cache ->
          let officialCache = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
              substituters = ["--option", "extra-substituters", toS (uri cachixOptions cache)]
              sigs = ["--option", "trusted-public-keys", officialCache <> " " <> toS (domain cachixOptions cache) <> "-1:" <> toS (WSS.publicKey cache)]
           in substituters ++ sigs
        Nothing -> []
      deploymentID = WSS.id (deploymentDetails :: WSS.DeploymentDetails)
      deploymentFinished hasSucceeded now =
        WSS.DeploymentFinished
          { WSS.id = deploymentID,
            WSS.time = now,
            WSS.hasSucceeded = hasSucceeded
          }
      deploymentFailed = do
        now <- liftIO getCurrentTime
        K.logLocM K.InfoS $ K.ls $ "Deploying #" <> index <> " failed."
        sendMessage $ deploymentFinished False now
        -- hack to flush logs
        liftIO $ threadDelay (1 * 1000 * 1000)

  K.logLocM K.InfoS $ K.ls $ "Deploying #" <> index <> ": " <> WSS.storePath deploymentDetails

  -- notify the service deployment started
  now <- liftIO getCurrentTime
  sendMessage $
    WSS.DeploymentStarted
      { WSS.id = deploymentID,
        WSS.time = now
      }

  -- TODO: don't create tmpfile for public caches
  -- TODO: add GC root so it's preserved for the next command
  -- get the store path using caches
  (downloadExitCode, _, _) <- liftIO $
    withSystemTempDirectory "netrc" $ \dir -> do
      let filepath = dir <> "netrc"
      args <- case WSS.cache agentInfo of
        Just cache -> do
          -- TODO: ugh
          let bc =
                BinaryCache.BinaryCache
                  { BinaryCache.name = "",
                    BinaryCache.uri = toS (uri cachixOptions cache),
                    BinaryCache.publicSigningKeys = [],
                    BinaryCache.isPublic = WSS.isPublic cache,
                    BinaryCache.githubUsername = "",
                    BinaryCache.permission = Read
                  }
          NetRc.add (Token agentToken) [bc] filepath
          return $ cachesArgs <> ["--option", "netrc-file", filepath]
        Nothing ->
          return cachesArgs
      shellOut "nix-store" (["-r", toS storePath] <> args)

  -- TODO: use exceptions to simplify this code
  case downloadExitCode of
    ExitFailure _ -> deploymentFailed
    ExitSuccess -> do
      let profile = AgentOptions.getProfile agentArgs
      -- TODO: document what happens if wrong user is used for the agent

      -- set the new profile
      (activateProfileExitCode, _, _) <- liftIO $ shellOut "nix-env" ["-p", toS profile, "--set", toS storePath]
      case activateProfileExitCode of
        ExitFailure _ -> deploymentFailed
        ExitSuccess -> do
          -- activate configuration
          maybeActivationScript <- liftIO $ getActivationScript storePath

          (activateScriptExitCode, _, _) <- case maybeActivationScript of
            Nothing -> return (ExitSuccess, (), ())
            Just activationScript -> liftIO activationScript

          case activateScriptExitCode of
            ExitFailure _ -> deploymentFailed
            ExitSuccess -> do
              now <- liftIO getCurrentTime
              sendMessage $ deploymentFinished True now

              liftIO $ log "Successfully activated the deployment."

              -- TODO: this is a hack to make sure the deployment is finished
              liftIO $ threadDelay (2 * 1000 * 1000)

              K.logLocM K.InfoS $ K.ls $ "Deployment #" <> index <> " finished"
  where
    -- TODO: prevent service from being restarted while deploying
    -- TODO: upgrade agent

    index :: Text
    index = show $ WSS.index deploymentDetails

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

    getActivationScript :: Text -> IO (Maybe (IO (ExitCode, (), ())))
    getActivationScript storePath = do
      isNixOS <- doesPathExist $ toS $ storePath <> "/nixos-version"
      if isNixOS
        then return $ Just $ shellOut (toS $ storePath <> "/bin/switch-to-configuration") ["switch"]
        else return Nothing

-- TODO: nix-darwin, home-manager
