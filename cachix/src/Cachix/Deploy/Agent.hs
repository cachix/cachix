{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Agent where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.Config as Config
import Cachix.Client.URI (getBaseUrl)
import qualified Cachix.Deploy.Log as Log
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
  Log.withLog logOptions $ \logEnv -> do
    CachixWebsocket.runForever logEnv options handleMessage
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
    profile = fromMaybe "system" (AgentOptions.profile agentOpts)
    options =
      CachixWebsocket.Options
        { CachixWebsocket.host = host,
          CachixWebsocket.name = name,
          CachixWebsocket.path = "/ws",
          CachixWebsocket.profile = profile,
          CachixWebsocket.isVerbose = Config.verbose cachixOptions
        }

    handleMessage :: ByteString -> Log.WithLog -> WS.Connection -> CachixWebsocket.AgentState -> ByteString -> K.KatipContextT IO ()
    handleMessage payload _ _ agentState _ = do
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
                    logOptions = logOptions,
                    websocketOptions =
                      CachixWebsocket.Options
                        { host = host,
                          name = name,
                          path = "/ws-deployment",
                          profile = profile,
                          isVerbose = Config.verbose cachixOptions
                        }
                  }
