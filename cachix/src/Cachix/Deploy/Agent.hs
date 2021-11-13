{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Agent where

import qualified Cachix.API.Deploy as API
import Cachix.API.Error (escalate)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.Env as Env
import qualified Cachix.Client.OptionsParser as CachixOptions
import Cachix.Client.Retry
import Cachix.Client.Servant (deployClient)
import Cachix.Client.URI (getBaseUrl)
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.Activate as Activate
import qualified Cachix.Deploy.OptionsParser as AgentOptions
import qualified Cachix.Types.ByteStringStreaming
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Control.Exception.Safe as Exception
import Data.Coerce (coerce)
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.TQueue as Conduit
import Data.IORef
import Katip (KatipContextT)
import qualified Katip as K
import qualified Network.WebSockets as WS
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client (Token (Token))
import Servant.Client (ClientEnv)
import qualified Servant.Client as Servant
import Servant.Client.Streaming (ClientM, client, runClientM)
import System.Environment (getEnv)
import qualified Wuss

type AgentState = IORef (Maybe WSS.AgentInformation)

run :: CachixOptions.CachixOptions -> AgentOptions.AgentOptions -> IO ()
run cachixOptions agentOpts = withKatip (CachixOptions.verbose cachixOptions) $ \logEnv -> do
  agentToken <- getEnv "CACHIX_AGENT_TOKEN"
  -- TODO: error if token is missing
  agentState <- newIORef Nothing
  clientEnv <- Env.createClientEnv cachixOptions
  let runKatip = K.runKatipContextT logEnv () "agent"
      headers =
        [ ("Authorization", "Basic " <> toS agentToken),
          ("name", name),
          ("version", toS versionNumber)
        ]
      host = Servant.baseUrlHost $ getBaseUrl $ CachixOptions.host cachixOptions
      path = "/ws"
      logger _ exception _ = runKatip $ K.logLocM K.ErrorS $ K.ls $ "Retrying due to an exception:" <> displayException exception
  runKatip $
    retryAllWithLogging endlessRetryPolicy logger $ do
      K.logLocM K.InfoS $ K.ls ("Agent " <> agentIdentifier <> " connecting to " <> toS host <> path)
      liftIO $
        Wuss.runSecureClientWith host 443 (toS path) WS.defaultConnectionOptions headers $ \connection -> runKatip $ do
          K.logLocM K.InfoS "Connected to Cachix Deploy service"
          liftIO $
            WS.withPingThread connection 30 (runKatip $ K.logLocM K.DebugS "WebSocket keep-alive ping") $ do
              WSS.recieveDataConcurrently
                connection
                (\message -> Exception.handle (handler runKatip) $ runKatip (handleMessage runKatip clientEnv (toS agentToken) message connection agentState))
  where
    name = toS (AgentOptions.name agentOpts)
    agentIdentifier = name <> " " <> toS versionNumber
    handleMessage :: (KatipContextT IO () -> IO ()) -> ClientEnv -> ByteString -> ByteString -> WS.Connection -> AgentState -> KatipContextT IO ()
    handleMessage runKatip clientEnv agentToken payload connection agentState = do
      case WSS.parseMessage payload of
        (Left err) ->
          -- TODO: show the bytestring?
          K.logLocM K.ErrorS $ K.ls $ "Failed to parse websocket payload: " <> err
        (Right message) ->
          case WSS.command message of
            WSS.AgentRegistered agentInformation -> do
              K.logLocM K.InfoS $ K.ls $ "Agent " <> agentIdentifier <> " registered."
              liftIO $ atomicWriteIORef agentState (Just agentInformation)
            WSS.Deployment deploymentDetails -> do
              maybeAgentInformation <- liftIO $ readIORef agentState
              let index :: Text
                  index = show $ WSS.index deploymentDetails
                  deploymentID = WSS.id (deploymentDetails :: WSS.DeploymentDetails)
              case maybeAgentInformation of
                Nothing -> K.logLocM K.InfoS $ K.ls $ "Ignoring deployment #" <> index <> " as agent isn't registered yet."
                Just agentInformation -> do
                  queue <- liftIO $ atomically TQueue.newTQueue
                  let consumer =
                        --Conduit.connect (Conduit.sourceTQueue queue) Conduit.print
                        escalate <=< (`runClientM` clientEnv) $ API.streamLog deployClient (Token agentToken) deploymentID (Conduit.mapOutput coerce $ Conduit.sourceTQueue queue)
                  liftIO $ Async.concurrently_ consumer $ runKatip $ Activate.activate agentOpts connection (Conduit.sinkTQueue queue) deploymentDetails agentInformation

handler :: (KatipContextT IO () -> IO ()) -> Exception.SomeException -> IO ()
handler runKatip e = do
  runKatip $ K.logLocM K.ErrorS $ "Unexpected exception: " <> K.ls (Exception.displayException e)

withKatip :: Bool -> (K.LogEnv -> IO a) -> IO a
withKatip isVerbose =
  bracket createLogEnv K.closeScribes
  where
    permit = if isVerbose then K.DebugS else K.InfoS
    createLogEnv = do
      logEnv <- K.initLogEnv "agent" "production"
      stdoutScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem permit) K.V2
      K.registerScribe "stdout" stdoutScribe K.defaultScribeSettings logEnv
