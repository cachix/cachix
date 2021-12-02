{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Agent where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.OptionsParser as CachixOptions
import Cachix.Client.Retry
import Cachix.Client.URI (getBaseUrl)
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.Activate as Activate
import qualified Cachix.Deploy.OptionsParser as AgentOptions
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
  let runKatip = K.runKatipContextT logEnv () "agent"
      headers =
        [ ("Authorization", "Basic " <> toS agentToken),
          ("name", name),
          ("version", toS versionNumber)
        ]
      host = Servant.baseUrlHost $ getBaseUrl $ CachixOptions.host cachixOptions
      path = "/ws"
  runKatip $
    retryAllWithLogging endlessRetryPolicy (logger runKatip) $ do
      K.logLocM K.InfoS $ K.ls ("Agent " <> agentIdentifier <> " connecting to " <> toS host <> toS path)
      liftIO $
        Wuss.runSecureClientWith host 443 path WS.defaultConnectionOptions headers $ \connection -> runKatip $ do
          K.logLocM K.InfoS "Connected to Cachix Deploy service"
          liftIO $
            WS.withPingThread connection 30 (runKatip $ K.logLocM K.DebugS "WebSocket keep-alive ping") $ do
              WSS.recieveDataConcurrently
                connection
                (\message -> Exception.handle (handler runKatip) $ runKatip (handleMessage runKatip host headers message connection agentState))
  where
    name = toS (AgentOptions.name agentOpts)
    agentIdentifier = name <> " " <> toS versionNumber
    logger runKatip _ exception _ = runKatip $ K.logLocM K.ErrorS $ K.ls $ "Retrying due to an exception:" <> displayException exception
    handleMessage :: (KatipContextT IO () -> IO ()) -> String -> RequestHeaders -> ByteString -> WS.Connection -> AgentState -> KatipContextT IO ()
    handleMessage runKatip host headers payload connection agentState = do
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
                  liftIO $ Async.concurrently_ (runLogStreaming runKatip host headers queue deploymentID) $ runKatip $ Activate.activate cachixOptions agentOpts connection (Conduit.sinkTQueue queue) deploymentDetails agentInformation

    runLogStreaming :: (KatipContextT IO () -> IO ()) -> String -> RequestHeaders -> Conduit.TQueue ByteString -> UUID -> IO ()
    runLogStreaming runKatip host headers queue deploymentID = do
      -- TODO: debug Conduit.print
      let path = "/api/v1/deploy/log/" <> UUID.toText deploymentID
      retryAllWithLogging endlessRetryPolicy (logger runKatip) $ do
        liftIO $
          Wuss.runSecureClientWith host 443 (toS path) WS.defaultConnectionOptions headers $ \connection ->
            Conduit.runConduit $
              Conduit.sourceTQueue queue
                .| Conduit.linesUnboundedAscii
                .|
                -- TODO: prepend timestamp
                websocketSend connection

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
      WS.sendTextData connection $ Aeson.encode $ Log $ toS bs

data Log = Log
  { line :: Text
  }
  deriving (Generic, Show, Aeson.ToJSON)
