{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Agent where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.Config as Config
import Cachix.Client.URI (getBaseUrl)
import qualified Cachix.Deploy.OptionsParser as AgentOptions
import Cachix.Deploy.StdinProcess (readProcess)
import qualified Cachix.Deploy.Websocket as CachixWebsocket
import qualified Data.Aeson as Aeson
import qualified Katip as K
import qualified Network.WebSockets as WS
import Paths_cachix (getBinDir)
import Protolude hiding (toS)
import Protolude.Conv
import qualified Servant.Client as Servant

run :: Config.CachixOptions -> AgentOptions.AgentOptions -> IO ()
run cachixOptions agentOpts =
  CachixWebsocket.runForever options handleMessage
  where
    host = toS $ Servant.baseUrlHost $ getBaseUrl $ Config.host cachixOptions
    name = AgentOptions.name agentOpts
    profile = fromMaybe "system" (AgentOptions.profile agentOpts)
    options =
      CachixWebsocket.Options
        { CachixWebsocket.host = host,
          CachixWebsocket.name = name,
          CachixWebsocket.path = "/ws",
          CachixWebsocket.profile = profile,
          CachixWebsocket.isVerbose = Config.verbose cachixOptions
        }
    handleMessage :: ByteString -> (K.KatipContextT IO () -> IO ()) -> WS.Connection -> CachixWebsocket.AgentState -> ByteString -> K.KatipContextT IO ()
    handleMessage payload _ _ agentState _ =
      CachixWebsocket.parseMessage payload (handleCommand . WSS.command)
      where
        handleCommand :: WSS.BackendCommand -> K.KatipContextT IO ()
        handleCommand (WSS.AgentRegistered agentInformation) =
          CachixWebsocket.registerAgent agentState agentInformation
        handleCommand (WSS.Deployment deploymentDetails) =
          liftIO $ do
            binDir <- toS <$> getBinDir
            readProcess (binDir <> "/.cachix-deployment") [] $
              toS . Aeson.encode $
                CachixWebsocket.Input
                  { deploymentDetails = deploymentDetails,
                    websocketOptions =
                      CachixWebsocket.Options
                        { host = host,
                          name = name,
                          path = "/ws-deployment",
                          profile = profile,
                          isVerbose = Config.verbose cachixOptions
                        }
                  }
