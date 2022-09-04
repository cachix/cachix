{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Agent where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.Config as Config
import Cachix.Client.URI (getBaseUrl)
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Deploy.OptionsParser as AgentOptions
import Cachix.Deploy.StdinProcess (readProcess)
import qualified Cachix.Deploy.Websocket as WebSocket
import qualified Data.Aeson as Aeson
import qualified Katip as K
import qualified Network.WebSockets as WS
import Paths_cachix (getBinDir)
import Protolude hiding (toS)
import Protolude.Conv
import qualified Servant.Client as Servant

data Deployment = Deployment
  { profileName :: Text,
    deploymentDetails :: WSS.DeploymentDetails,
    logOptions :: Log.Options,
    websocketOptions :: WebSocket.Options
  }
  deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

run :: Config.CachixOptions -> AgentOptions.AgentOptions -> IO ()
run cachixOptions agentOpts =
  Log.withLog logOptions $ \withLog -> do
    WebSocket.runForever withLog options (handleMessage withLog)
  where
    verbosity =
      if Config.verbose cachixOptions
        then Log.Verbose
        else Log.Normal

    logOptions =
      Log.Options
        { verbosity = verbosity,
          namespace = "agent",
          environment = "production"
        }

    host = toS $ Servant.baseUrlHost $ getBaseUrl $ Config.host cachixOptions
    name = AgentOptions.name agentOpts
    profileName = fromMaybe "system" (AgentOptions.profile agentOpts)
    options =
      WebSocket.Options
        { WebSocket.host = host,
          WebSocket.name = name,
          WebSocket.path = "/ws"
        }

    handleMessage :: Log.WithLog -> WS.Connection -> ByteString -> WebSocket.AgentState -> ByteString -> IO ()
    handleMessage withLog _ payload agentState _ =
      case WSS.parseMessage payload of
        Left err ->
          -- TODO: show the bytestring?
          withLog $ K.logLocM K.ErrorS $ K.ls $ "Failed to parse websocket payload: " <> err
        Right message ->
          handleCommand (WSS.command message)
      where
        handleCommand :: WSS.BackendCommand -> IO ()
        handleCommand (WSS.AgentRegistered agentInformation) =
          withLog $ WebSocket.registerAgent agentState agentInformation
        handleCommand (WSS.Deployment deploymentDetails) = do
          binDir <- toS <$> getBinDir
          readProcess (binDir <> "/.cachix-deployment") [] $
            toS . Aeson.encode $
              Deployment
                { profileName = profileName,
                  deploymentDetails = deploymentDetails,
                  logOptions = logOptions,
                  websocketOptions =
                    WebSocket.Options
                      { host = host,
                        name = name,
                        path = "/ws-deployment"
                      }
                }
