-- high level interface for websocket clients
module Cachix.Deploy.Websocket where

import Cachix.API.WebSocketSubprotocol (AgentInformation)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import Cachix.Client.Retry
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.WebsocketPong as WebsocketPong
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef
import qualified Katip as K
import Network.HTTP.Types (Header)
import qualified Network.WebSockets as WS
import Protolude hiding (toS)
import Protolude.Conv
import System.Environment (getEnv)
import qualified Wuss

type AgentState = IORef (Maybe WSS.AgentInformation)

data Options = Options
  { host :: Text,
    path :: Text,
    name :: Text,
    isVerbose :: Bool,
    profile :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Input = Input
  { deploymentDetails :: WSS.DeploymentDetails,
    websocketOptions :: Options
  }
  deriving (Show, Generic, ToJSON, FromJSON)

runForever :: Options -> (ByteString -> (K.KatipContextT IO () -> IO ()) -> WS.Connection -> AgentState -> ByteString -> K.KatipContextT IO ()) -> IO ()
runForever options cmd = withKatip (isVerbose options) $ \logEnv -> do
  agentToken <- getEnv "CACHIX_AGENT_TOKEN"
  -- TODO: error if token is missing
  agentState <- newIORef Nothing
  pongState <- WebsocketPong.newState
  mainThreadID <- myThreadId
  let runKatip = K.runKatipContextT logEnv () "agent"
      pingHandler = do
        last <- WebsocketPong.secondsSinceLastPong pongState
        runKatip $ K.logLocM K.DebugS $ K.ls $ "Sending WebSocket keep-alive ping, last pong was " <> (show last :: Text) <> " seconds ago"
        WebsocketPong.pingHandler pongState mainThreadID pongTimeout
      connectionOptions = WebsocketPong.installPongHandler pongState WS.defaultConnectionOptions
  runKatip $
    retryAllWithLogging endlessRetryPolicy (logger runKatip) $
      do
        K.logLocM K.InfoS $ K.ls ("Agent " <> agentIdentifier <> " connecting to " <> toS (host options) <> toS (path options))
        liftIO $ do
          -- refresh pong state in case we're reconnecting
          WebsocketPong.pongHandler pongState
          Wuss.runSecureClientWith (toS $ host options) 443 (toS $ path options) connectionOptions (headers options (toS agentToken)) $ \connection -> runKatip $ do
            K.logLocM K.InfoS "Connected to Cachix Deploy service"
            liftIO $
              WS.withPingThread connection pingEvery pingHandler $
                do
                  WSS.recieveDataConcurrently
                    connection
                    (\message -> runKatip (cmd message runKatip connection agentState (toS agentToken)))
  where
    agentIdentifier = name options <> " " <> toS versionNumber
    pingEvery = 30
    pongTimeout = pingEvery * 2

headers :: Options -> ByteString -> [Header]
headers options agentToken =
  [ ("Authorization", "Bearer " <> toS agentToken),
    ("name", toS (name options)),
    ("version", toS versionNumber)
  ]

-- TODO: log the exception
logger runKatip _ exception _ = runKatip $ K.logLocM K.ErrorS $ K.ls $ "Retrying due to an exception: " <> displayException exception

withKatip :: Bool -> (K.LogEnv -> IO a) -> IO a
withKatip isVerbose =
  bracket createLogEnv K.closeScribes
  where
    permit = if isVerbose then K.DebugS else K.InfoS
    createLogEnv = do
      logEnv <- K.initLogEnv "agent" "production"
      stdoutScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem permit) K.V2
      K.registerScribe "stdout" stdoutScribe K.defaultScribeSettings logEnv

parseMessage :: FromJSON cmd => ByteString -> (WSS.Message cmd -> K.KatipContextT IO ()) -> K.KatipContextT IO ()
parseMessage payload m = do
  case WSS.parseMessage payload of
    (Left err) ->
      -- TODO: show the bytestring?
      K.logLocM K.ErrorS $ K.ls $ "Failed to parse websocket payload: " <> err
    (Right message) ->
      m message

-- commands

registerAgent :: AgentState -> AgentInformation -> K.KatipContextT IO ()
registerAgent agentState agentInformation = do
  K.logLocM K.InfoS "Agent registered."
  liftIO $ atomicWriteIORef agentState (Just agentInformation)
