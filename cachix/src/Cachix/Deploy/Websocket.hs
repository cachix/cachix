-- high level interface for websocket clients
module Cachix.Deploy.Websocket where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.OptionsParser as CachixOptions
import Cachix.Client.Retry
import Cachix.Client.URI (getBaseUrl)
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.OptionsParser as AgentOptions
import qualified Cachix.Deploy.WebsocketPong as WebsocketPong
import qualified Control.Exception.Safe as Exception
import Data.IORef
import Data.String (String)
import qualified Katip as K
import Network.HTTP.Simple (RequestHeaders)
import qualified Network.WebSockets as WS
import Protolude hiding (toS)
import Protolude.Conv
import qualified Servant.Client as Servant
import System.Environment (getEnv)
import qualified Wuss

type AgentState = IORef (Maybe WSS.AgentInformation)

-- TODO: how to pass cmd options to the worker?
-- TODO: agent name
runForever :: CachixOptions.CachixOptions -> AgentOptions.AgentOptions -> ((K.KatipContextT IO () -> IO ()) -> String -> RequestHeaders -> ByteString -> WS.Connection -> AgentState -> ByteString -> K.KatipContextT IO ()) -> IO ()
runForever cachixOptions agentOpts cmd = withKatip (CachixOptions.verbose cachixOptions) $ \logEnv -> do
  agentToken <- getEnv "CACHIX_AGENT_TOKEN"
  -- TODO: error if token is missing
  agentState <- newIORef Nothing
  pongState <- WebsocketPong.newState
  mainThreadID <- myThreadId
  let runKatip = K.runKatipContextT logEnv () "agent"
      headers =
        [ ("Authorization", "Bearer " <> toS agentToken),
          ("name", name),
          ("version", toS versionNumber)
        ]
      pingHandler = do
        last <- WebsocketPong.secondsSinceLastPong pongState
        runKatip $ K.logLocM K.DebugS $ K.ls $ "Sending WebSocket keep-alive ping, last pong was " <> (show last :: Text) <> " seconds ago"
        WebsocketPong.pingHandler pongState mainThreadID pongTimeout
      connectionOptions = WebsocketPong.installPongHandler pongState WS.defaultConnectionOptions
  runKatip $
    retryAllWithLogging endlessRetryPolicy (logger runKatip) $ do
      K.logLocM K.InfoS $ K.ls ("Agent " <> agentIdentifier <> " connecting to " <> toS host <> toS path)
      liftIO $ do
        -- refresh pong state in case we're reconnecting
        WebsocketPong.pongHandler pongState
        Wuss.runSecureClientWith host 443 path connectionOptions headers $ \connection -> runKatip $ do
          K.logLocM K.InfoS "Connected to Cachix Deploy service"
          liftIO $
            WS.withPingThread connection pingEvery pingHandler $ do
              WSS.recieveDataConcurrently
                connection
                (\message -> Exception.handle (handler runKatip) $ runKatip (cmd runKatip host headers message connection agentState (toS agentToken)))
  where
    agentIdentifier = name <> " " <> toS versionNumber
    name = toS (AgentOptions.name agentOpts)
    path = "/ws"
    pingEvery = 30
    pongTimeout = pingEvery * 2
    host = Servant.baseUrlHost $ getBaseUrl $ CachixOptions.host cachixOptions

-- TODO: log the exception
handler :: (K.KatipContextT IO () -> IO ()) -> Exception.SomeException -> IO ()
handler runKatip e = do
  runKatip $ K.logLocM K.ErrorS $ "Unexpected exception: " <> K.ls (Exception.displayException e)

logger runKatip _ exception _ = runKatip $ K.logLocM K.ErrorS $ K.ls $ "Retrying due to an exception:" <> displayException exception

withKatip :: Bool -> (K.LogEnv -> IO a) -> IO a
withKatip isVerbose =
  bracket createLogEnv K.closeScribes
  where
    permit = if isVerbose then K.DebugS else K.InfoS
    createLogEnv = do
      logEnv <- K.initLogEnv "agent" "production"
      stdoutScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem permit) K.V2
      K.registerScribe "stdout" stdoutScribe K.defaultScribeSettings logEnv
