{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Activate where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.OptionsParser as CachixOptions
import Cachix.Client.URI (getBaseUrl)
import qualified Cachix.Deploy.OptionsParser as AgentOptions
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.Process as Conduit
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID.V4 as UUID
import qualified Katip as K
import qualified Network.WebSockets as WS
import Protolude hiding (log, toS)
import Protolude.Conv (toS)
import qualified Servant.Client as Servant
import System.Directory (doesPathExist)
import System.Process

-- TODO: duplicated in Agent.hs
host cachixOptions = Servant.baseUrlHost $ getBaseUrl $ CachixOptions.host cachixOptions

-- TODO: what if websocket gets closed while deploying?
activate ::
  CachixOptions.CachixOptions ->
  AgentOptions.AgentOptions ->
  WS.Connection ->
  Conduit.ConduitT ByteString Void IO () ->
  WSS.DeploymentDetails ->
  WSS.AgentInformation ->
  K.KatipContextT IO ()
activate cachixOptions agentArgs connection sourceStream deploymentDetails agentInfo = do
  let storePath = WSS.storePath deploymentDetails
      cachesArgs = case WSS.cache agentInfo of
        Just cache ->
          let domain = toS (WSS.cacheName cache) <> "." <> toS (host cachixOptions)
              officialCache = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
              -- TODO: get uri scheme
              substituters = ["--option", "extra-substituters", "https://" <> domain]
              sigs = ["--option", "trusted-public-keys", officialCache <> " " <> domain <> "-1:" <> toS (WSS.publicKey cache)]
           in -- TODO: netrc if WSS.isPublic
              substituters ++ sigs
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

  -- TODO: full url to the deployment page
  K.logLocM K.InfoS $ K.ls $ "Deploying #" <> index <> ": " <> WSS.storePath deploymentDetails

  -- notify the service deployment started
  now <- liftIO getCurrentTime
  sendMessage $
    WSS.DeploymentStarted
      { WSS.id = deploymentID,
        WSS.time = now
      }

  -- get the store path using caches
  (downloadExitCode, _, _) <- liftIO $ shellOut "nix-store" (["-r", toS storePath] <> cachesArgs)

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

              K.logLocM K.InfoS $ K.ls $ "Deployment #" <> index <> " finished"
  where
    -- TODO: prevent service from being restarted while deploying
    -- TODO: upgrade agent

    index :: Text
    index = show $ WSS.index deploymentDetails

    shellOut cmd args = do
      log $ "\nRunning: $ " <> toS cmd <> " " <> toS (unwords $ fmap toS args)
      Conduit.sourceProcessWithStreams (proc cmd args) Conduit.sinkNull sourceStream sourceStream

    log :: ByteString -> IO ()
    log msg = Conduit.connect (Conduit.yieldMany [msg <> "\n"]) sourceStream

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
