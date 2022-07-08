{-# LANGUAGE DuplicateRecordFields #-}

module Main
  ( main,
  )
where

import Cachix.API.Error (escalateAs)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import Cachix.Client.Retry
import qualified Cachix.Deploy.Activate as Activate
import qualified Cachix.Deploy.Websocket as CachixWebsocket
import Conduit ((.|))
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Data.Aeson as Aeson
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.TQueue as Conduit
import Data.String (String)
import Data.Time.Clock (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import GHC.IO.Encoding
import qualified Katip as K
import Network.HTTP.Simple (RequestHeaders)
import qualified Network.WebSockets as WS
import Protolude hiding (toS)
import Protolude.Conv
import System.IO (BufferMode (..), hSetBuffering)
import qualified Wuss

-- TODO: https://superuser.com/questions/1333069/how-to-track-all-child-processes-spawned-by-a-systemctl-service

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  input <- escalateAs (FatalError . toS) . Aeson.eitherDecode . toS =<< getContents
  CachixWebsocket.runForever (CachixWebsocket.websocketOptions input) (handleMessage input)

handleMessage :: CachixWebsocket.Input -> ByteString -> (K.KatipContextT IO () -> IO ()) -> WS.Connection -> CachixWebsocket.AgentState -> ByteString -> K.KatipContextT IO ()
handleMessage input payload runKatip connection _ agentToken =
  CachixWebsocket.parseMessage payload (handleCommand . WSS.command)
  where
    deploymentDetails = CachixWebsocket.deploymentDetails input
    options = CachixWebsocket.websocketOptions input
    handleCommand :: WSS.BackendCommand -> K.KatipContextT IO ()
    handleCommand (WSS.Deployment _) =
      K.logLocM K.ErrorS "cachix-deployment should have never gotten a deployment command directly."
    handleCommand (WSS.AgentRegistered agentInformation) = do
      queue <- liftIO $ atomically TQueue.newTQueue
      let deploymentID = WSS.id (deploymentDetails :: WSS.DeploymentDetails)
          streamingThread = runLogStreaming (toS $ CachixWebsocket.host options) (CachixWebsocket.headers options agentToken) queue deploymentID
          activateThread = runKatip $ do
            Activate.activate options connection (Conduit.sinkTQueue queue) deploymentDetails agentInformation agentToken
      liftIO $ Async.race_ streamingThread activateThread
      throwIO ExitSuccess
    runLogStreaming :: String -> RequestHeaders -> Conduit.TQueue ByteString -> UUID -> IO ()
    runLogStreaming host headers queue deploymentID = do
      let path = "/api/v1/deploy/log/" <> UUID.toText deploymentID
      retryAllWithLogging endlessRetryPolicy (CachixWebsocket.logger runKatip) $ do
        liftIO $
          Wuss.runSecureClientWith host 443 (toS path) WS.defaultConnectionOptions headers $
            \conn ->
              bracket_ (return ()) (WS.sendClose connection ("Closing." :: ByteString)) $
                Conduit.runConduit $
                  Conduit.sourceTQueue queue
                    .| Conduit.linesUnboundedAscii
                    -- TODO: prepend katip-like format to each line
                    -- .| (if CachixWebsocket.isVerbose options then Conduit.print else mempty)
                    .| sendLog conn

sendLog :: WS.Connection -> Conduit.ConduitT ByteString Conduit.Void IO ()
sendLog connection = Conduit.mapM_ f
  where
    f = \bs -> do
      now <- getCurrentTime
      WS.sendTextData connection $ Aeson.encode $ WSS.Log {WSS.line = toS bs, WSS.time = now}
