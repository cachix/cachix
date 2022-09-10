{-# LANGUAGE ScopedTypeVariables #-}

-- high level interface for websocket clients
module Cachix.Deploy.Websocket where

import qualified Cachix.Client.Retry as Retry
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Deploy.WebsocketPong as WebsocketPong
import Control.Exception.Safe (Handler (..), MonadMask, isSyncException)
import qualified Control.Exception.Safe as Safe
import qualified Control.Retry as Retry
import Data.String (String)
import qualified Katip as K
import qualified Network.HTTP.Simple as HTTP
import qualified Network.WebSockets as WS
import Protolude hiding (Handler, toS)
import Protolude.Conv
import qualified System.Info
import qualified System.Timeout as Timeout
import qualified Wuss

data Options = Options
  { host :: Text,
    path :: Text,
    headers :: HTTP.RequestHeaders,
    -- | The identifier used when logging. Usually a combination of the agent
    -- name and the CLI version.
    agentIdentifier :: Text
  }
  deriving (Show)

system :: String
system = System.Info.arch <> "-" <> System.Info.os

withConnection ::
  -- | Logging context for logging socket status
  Log.WithLog ->
  -- | WebSocket options
  Options ->
  -- | The app to run inside the socket
  WS.ClientApp () ->
  IO ()
withConnection withLog options app = do
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
    Wuss.runSecureClientWith (toS $ host options) 443 (toS $ path options) connectionOptions (headers options) $
      \connection ->
        do
          withLog $ K.logLocM K.InfoS "Connected to Cachix Deploy service"
          WS.withPingThread connection pingEvery pingHandler $
            app connection
          `finally` waitForGracefulShutdown connection

-- Log all exceptions and retry, except when the websocket receives a close request.
-- TODO: use exponential retry with reset: https://github.com/Soostone/retry/issues/25
reconnectWithLog :: (MonadMask m, MonadIO m) => Log.WithLog -> m () -> m ()
reconnectWithLog withLog inner =
  Safe.handle closeRequest $
    Retry.recovering Retry.endlessConstantRetryPolicy handlers $ const inner
  where
    closeRequest (WS.CloseRequest _ _) = return ()
    closeRequest e = Safe.throwM e

    handlers = Retry.skipAsyncExceptions ++ [exitOnSuccess, exitOnCloseRequest, logSyncExceptions]

    exitOnSuccess _ = Handler $ \(_ :: ExitCode) -> return False

    exitOnCloseRequest _ = Handler $ \(e :: WS.ConnectionException) ->
      case e of
        WS.CloseRequest _ _ -> do
          liftIO . withLog $
            K.logLocM K.DebugS . K.ls $
              ("Received close request from peer. Closing connection" :: Text)
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
    delay (Just t) = show (toSeconds t) <> " seconds"

    toSeconds :: Int -> Int
    toSeconds t =
      floor $ (fromIntegral t :: Double) / 1000 / 1000

-- | Try to gracefully close the WebSocket.
--
-- Do not run with asynchronous exceptions masked, ie. Control.Exception.Safe.finally.
--
-- We send a close request to the peer and continue processing
-- any incoming messages until the server replies with its own
-- close control message.
waitForGracefulShutdown :: WS.Connection -> IO ()
waitForGracefulShutdown connection = do
  WS.sendClose connection ("Closing." :: ByteString)

  -- Grace period
  response <- Timeout.timeout (5 * 1000 * 1000) $ forever (WS.receiveDataMessage connection)

  when (isNothing response) $
    throwIO $ WS.CloseRequest 1000 "No response to close request"

createHeaders ::
  -- | Agent name
  Text ->
  -- | Agent Token
  Text ->
  HTTP.RequestHeaders
createHeaders agentName agentToken =
  [ ("Authorization", "Bearer " <> toS agentToken),
    ("name", toS agentName),
    ("version", toS versionNumber),
    ("system", toS system)
  ]
