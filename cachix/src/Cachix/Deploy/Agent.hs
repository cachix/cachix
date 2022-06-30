{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Agent where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.OptionsParser as CachixOptions
import Cachix.Client.Retry
import Cachix.Client.URI (getBaseUrl)
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.Activate as Activate
import qualified Cachix.Deploy.OptionsParser as AgentOptions
import qualified Cachix.Deploy.WebsocketPong as WebsocketPong
import Conduit ((.|))
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.TQueue as Conduit
import Data.IORef
import Data.String (String)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Katip (KatipContextT)
import qualified Katip as K
import Network.HTTP.Simple (RequestHeaders)
import qualified Network.WebSockets as WS
import Protolude hiding (toS)
import Protolude.Conv
import qualified Servant.Client as Servant
import System.Environment (getEnv)
import qualified Wuss

type AgentState = IORef (Maybe WSS.AgentInformation)

run :: CachixOptions.CachixOptions -> AgentOptions.AgentOptions -> IO ()
run cachixOptions agentOpts = withKatip (CachixOptions.verbose cachixOptions) $ \logEnv -> do
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
      host = Servant.baseUrlHost $ getBaseUrl $ CachixOptions.host cachixOptions
      path = "/ws"
      pingEvery = 30
      pongTimeout = pingEvery * 2
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
                (\message -> Exception.handle (handler runKatip) $ runKatip (handleMessage runKatip host headers message connection agentState (toS agentToken)))
  where
    name = toS (AgentOptions.name agentOpts)
    agentIdentifier = name <> " " <> toS versionNumber
    logger runKatip _ exception _ = runKatip $ K.logLocM K.ErrorS $ K.ls $ "Retrying due to an exception:" <> displayException exception
    handleMessage :: (KatipContextT IO () -> IO ()) -> String -> RequestHeaders -> ByteString -> WS.Connection -> AgentState -> ByteString -> KatipContextT IO ()
    handleMessage runKatip host headers payload connection agentState agentToken = do
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
                  liftIO $ Async.race_ (runLogStreaming runKatip host headers queue deploymentID) $ runKatip $ Activate.activate cachixOptions agentOpts connection (Conduit.sinkTQueue queue) deploymentDetails agentInformation agentToken

    runLogStreaming :: (KatipContextT IO () -> IO ()) -> String -> RequestHeaders -> Conduit.TQueue ByteString -> UUID -> IO ()
    runLogStreaming runKatip host headers queue deploymentID = do
      -- TODO: debug Conduit.print
      let path = "/api/v1/deploy/log/" <> UUID.toText deploymentID
      retryAllWithLogging endlessRetryPolicy (logger runKatip) $ do
        liftIO $
          Wuss.runSecureClientWith host 443 (toS path) WS.defaultConnectionOptions headers $ \connection ->
            bracket_ (return ()) (WS.sendClose connection ("Closing." :: ByteString)) $
              Conduit.runConduit $
                Conduit.sourceTQueue queue
                  .| Conduit.linesUnboundedAscii
                  .| websocketSend connection

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

websocketSend :: WS.Connection -> Conduit.ConduitT ByteString Conduit.Void IO ()
websocketSend connection = Conduit.mapM_ f
  where
    f = \bs -> do
      now <- getCurrentTime
      WS.sendTextData connection $ Aeson.encode $ WSS.Log {line = toS bs, time = now}
