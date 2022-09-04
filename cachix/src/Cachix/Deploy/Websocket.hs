{-# LANGUAGE ScopedTypeVariables #-}

-- high level interface for websocket clients
module Cachix.Deploy.Websocket where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.Retry as Retry
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Deploy.WebsocketPong as WebsocketPong
import Control.Exception.Safe (Handler (..), MonadMask, isSyncException)
import qualified Control.Retry as Retry
import qualified Data.Aeson as Aeson
import Data.String (String)
import qualified Katip as K
import Network.HTTP.Types (Header)
import qualified Network.WebSockets as WS
import Protolude hiding (Handler, toS)
import Protolude.Conv
import qualified System.Info
import qualified Wuss

data Options = Options
  { host :: Text,
    path :: Text,
    headers :: [Header],
    -- | The identifier used when logging. Usually a combination of the agent
    -- name and the CLI version.
    agentIdentifier :: Text
  }
  deriving (Show)

system :: String
system = System.Info.arch <> "-" <> System.Info.os

runForever ::
  -- | Logging context for logging socket status
  Log.WithLog ->
  -- | WebSocket options
  Options ->
  -- | The app to run inside the socket
  (WS.Connection -> ByteString -> IO ()) ->
  IO ()
runForever withLog options inner = do
  mainThreadID <- myThreadId
  pongState <- WebsocketPong.newState

  let pingEvery = 30
  let pongTimeout = pingEvery * 2
  let pingHandler = do
        last <- WebsocketPong.secondsSinceLastPong pongState
        withLog $ K.logLocM K.DebugS $ K.ls $ "Sending WebSocket keep-alive ping, last pong was " <> (show last :: Text) <> " seconds ago"
        WebsocketPong.pingHandler pongState mainThreadID pongTimeout
  let connectionOptions = WebsocketPong.installPongHandler pongState WS.defaultConnectionOptions

  reconnectWithLog withLog $ do
    withLog $
      K.logLocM K.InfoS $ K.ls $ "Agent " <> agentIdentifier options <> " connecting to " <> host options <> path options

    -- refresh pong state in case we're reconnecting
    WebsocketPong.pongHandler pongState

    -- TODO: https://github.com/jaspervdj/websockets/issues/229
    Wuss.runSecureClientWith (toS $ host options) 443 (toS $ path options) connectionOptions (headers options) $ \connection -> do
      withLog $ K.logLocM K.InfoS "Connected to Cachix Deploy service"
      WS.withPingThread connection pingEvery pingHandler $
        WSS.recieveDataConcurrently connection (inner connection)

-- TODO: use exponential retry with reset: https://github.com/Soostone/retry/issues/25
reconnectWithLog :: (MonadMask m, MonadIO m) => Log.WithLog -> m a -> m a
reconnectWithLog withLog inner =
  Retry.recovering Retry.endlessConstantRetryPolicy handlers $ const inner
  where
    handlers = Retry.skipAsyncExceptions ++ [exitOnSuccess, exitOnCloseRequest, logSyncExceptions]

    exitOnSuccess _ = Handler $ \(_ :: ExitCode) -> return False

    exitOnCloseRequest _ = Handler $ \(e :: WS.ConnectionException) ->
      case e of
        WS.CloseRequest _ _ -> do
          liftIO . withLog $
            K.logLocM K.DebugS . K.ls $
              ("Received close request from peer. Closing connection." :: Text)
          return False
        _ -> return True

    logSyncExceptions = Retry.logRetries (return . isSyncException) logRetries

    logRetries :: (MonadIO m) => Bool -> SomeException -> Retry.RetryStatus -> m ()
    logRetries _ exception retryStatus =
      liftIO . withLog $
        K.logLocM K.ErrorS . K.ls $
          "Retrying in " <> delay (Retry.rsPreviousDelay retryStatus) <> " due to an exception: " <> displayException exception

    delay :: Maybe Int -> String
    delay Nothing = "0 seconds"
    delay (Just s) = show (floor (fromIntegral s / 1000 / 1000)) <> " seconds"

createHeaders ::
  -- | Agent name
  Text ->
  -- | Agent Token
  Text ->
  [Header]
createHeaders agentName agentToken =
  [ ("Authorization", "Bearer " <> toS agentToken),
    ("name", toS agentName),
    ("version", toS versionNumber),
    ("system", toS system)
  ]

-- TODO: Move to the agent. If the websocket is going to parse messages then it
-- can’t also swallow errors like this.
parseMessage :: K.KatipContext m => Aeson.FromJSON cmd => ByteString -> (WSS.Message cmd -> m ()) -> m ()
parseMessage payload m = do
  case WSS.parseMessage payload of
    Left err ->
      -- TODO: show the bytestring?
      K.logLocM K.ErrorS $ K.ls $ "Failed to parse websocket payload: " <> err
    Right message ->
      m message
