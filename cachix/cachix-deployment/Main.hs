{-# LANGUAGE DuplicateRecordFields #-}

module Main
  ( main,
  )
where

import Cachix.API.Error (escalateAs)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import Cachix.Client.Retry
import qualified Cachix.Deploy.Activate as Activate
import qualified Cachix.Deploy.Lock as Lock
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Deploy.Websocket as CachixWebsocket
import Conduit ((.|))
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TMQueue as TMQueue
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

-- | Activate the new deployment.
--
-- If the target profile is already locked by another deployment, exit
-- immediately and rely on the backend to reschedule.
main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  input <- escalateAs (FatalError . toS) . Aeson.eitherDecode . toS =<< getContents

  let logOptions = CachixWebsocket.logOptions input
  let websocketOptions = CachixWebsocket.websocketOptions input
  let profile = CachixWebsocket.profile websocketOptions

  Log.withLog logOptions $ \logEnv ->
    void . Lock.withTryLock profile $
      CachixWebsocket.runForever logEnv websocketOptions (handleMessage input)

handleMessage :: CachixWebsocket.Input -> ByteString -> Log.WithLog -> WS.Connection -> CachixWebsocket.AgentState -> ByteString -> IO ()
handleMessage input payload withLog connection _ agentToken =
  case WSS.parseMessage payload of
    Left err ->
      -- TODO: show the bytestring?
      withLog $ K.logLocM K.ErrorS $ K.ls $ "Failed to parse websocket payload: " <> err
    Right message ->
      handleCommand (WSS.command message)
  where
    deploymentDetails = CachixWebsocket.deploymentDetails input
    options = CachixWebsocket.websocketOptions input

    handleCommand :: WSS.BackendCommand -> IO ()
    handleCommand (WSS.Deployment _) =
      withLog $ K.logLocM K.ErrorS "cachix-deployment should have never gotten a deployment command directly."

    handleCommand (WSS.AgentRegistered agentInformation) = do
      queue <- atomically TMQueue.newTMQueue

      let deploymentID = WSS.id (deploymentDetails :: WSS.DeploymentDetails)
          streamingThread = runLogStreaming (toS $ CachixWebsocket.host options) (CachixWebsocket.headers options agentToken) queue deploymentID
          activateThread =
            withLog (Activate.activate options connection (Conduit.sinkTMQueue queue) deploymentDetails agentInformation agentToken)
              `finally` atomically (TMQueue.closeTMQueue queue)

      Async.concurrently_ streamingThread activateThread
      -- TODO: move this into a `finally` after refactoring
      WS.sendClose connection ("Closing." :: ByteString)
      throwIO ExitSuccess

    runLogStreaming :: String -> RequestHeaders -> TMQueue.TMQueue ByteString -> UUID -> IO ()
    runLogStreaming host headers queue deploymentID = do
      let path = "/api/v1/deploy/log/" <> UUID.toText deploymentID
      retryAllWithLogging endlessRetryPolicy (CachixWebsocket.logRetry withLog) $
        Wuss.runSecureClientWith host 443 (toS path) WS.defaultConnectionOptions headers $
          \conn ->
            Conduit.runConduit $
              Conduit.sourceTMQueue queue
                .| Conduit.linesUnboundedAscii
                -- TODO: prepend katip-like format to each line
                -- .| (if CachixWebsocket.isVerbose options then Conduit.print else mempty)
                .| sendLog conn

sendLog :: WS.Connection -> Conduit.ConduitT ByteString Conduit.Void IO ()
sendLog connection = Conduit.mapM_ $ \bs -> do
  now <- getCurrentTime
  WS.sendTextData connection $ Aeson.encode $ WSS.Log {WSS.line = toS bs, WSS.time = now}
