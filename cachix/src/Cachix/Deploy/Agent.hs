{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Agent where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.OptionsParser as CachixOptions
import Cachix.Client.Retry
import qualified Cachix.Deploy.Activate as Activate
import qualified Cachix.Deploy.OptionsParser as AgentOptions
import qualified Cachix.Deploy.Websocket as CachixWebsocket
import Conduit ((.|))
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Data.Aeson as Aeson
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.TQueue as Conduit
import Data.IORef
import Data.String (String)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Katip as K
import Network.HTTP.Simple (RequestHeaders)
import qualified Network.WebSockets as WS
import Protolude hiding (toS)
import Protolude.Conv
import qualified Wuss

run :: CachixOptions.CachixOptions -> AgentOptions.AgentOptions -> IO ()
run cachixOptions agentOpts = do
  CachixWebsocket.runForever cachixOptions agentOpts handleMessage
  where
    handleMessage :: (K.KatipContextT IO () -> IO ()) -> String -> RequestHeaders -> ByteString -> WS.Connection -> CachixWebsocket.AgentState -> ByteString -> K.KatipContextT IO ()
    handleMessage runKatip host headers payload connection agentState agentToken = do
      case WSS.parseMessage payload of
        (Left err) ->
          -- TODO: show the bytestring?
          K.logLocM K.ErrorS $ K.ls $ "Failed to parse websocket payload: " <> err
        (Right message) ->
          case WSS.command message of
            WSS.AgentRegistered agentInformation -> do
              K.logLocM K.InfoS "Agent registered."
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

    runLogStreaming :: (K.KatipContextT IO () -> IO ()) -> String -> RequestHeaders -> Conduit.TQueue ByteString -> UUID -> IO ()
    runLogStreaming runKatip host headers queue deploymentID = do
      -- TODO: debug Conduit.print
      let path = "/api/v1/deploy/log/" <> UUID.toText deploymentID
      retryAllWithLogging endlessRetryPolicy (CachixWebsocket.logger runKatip) $ do
        liftIO $
          Wuss.runSecureClientWith host 443 (toS path) WS.defaultConnectionOptions headers $ \connection ->
            bracket_ (return ()) (WS.sendClose connection ("Closing." :: ByteString)) $
              Conduit.runConduit $
                Conduit.sourceTQueue queue
                  .| Conduit.linesUnboundedAscii
                  .| websocketSend connection
      where
        websocketSend :: WS.Connection -> Conduit.ConduitT ByteString Conduit.Void IO ()
        websocketSend connection = Conduit.mapM_ f
          where
            f = \bs -> do
              now <- getCurrentTime
              WS.sendTextData connection $ Aeson.encode $ WSS.Log {line = toS bs, time = now}
