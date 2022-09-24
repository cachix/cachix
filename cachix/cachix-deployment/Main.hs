{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Cachix.API.Error (escalateAs)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import Cachix.Client.Retry
import qualified Cachix.Deploy.Activate as Activate
import qualified Cachix.Deploy.Agent as Agent
import qualified Cachix.Deploy.Lock as Lock
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Deploy.Websocket as WebSocket
import Conduit ((.|))
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TMQueue as TMQueue
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.TQueue as Conduit
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
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
--
-- TODO: what if websocket gets closed while deploying?
main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  deployment <- escalateAs (FatalError . toS) . Aeson.eitherDecode . toS =<< getContents

  let Agent.Deployment
        { agentName,
          agentToken,
          profileName,
          host,
          logOptions
        } = deployment

  let headers = WebSocket.createHeaders agentName agentToken
  let websocketOptions =
        WebSocket.Options
          { WebSocket.host = host,
            WebSocket.path = "/ws-deployment",
            WebSocket.headers = headers,
            WebSocket.agentIdentifier = Agent.agentIdentifier agentName
          }

  Log.withLog logOptions $ \withLog ->
    void . Lock.withTryLock profileName $
      WebSocket.runForever withLog websocketOptions (handleMessage withLog deployment websocketOptions)

handleMessage ::
  -- | Logging context
  Log.WithLog ->
  -- | Deployment information passed from the agent
  Agent.Deployment ->
  -- | WebSocket options
  WebSocket.Options ->
  -- | Backend WebSocket connection
  WS.Connection ->
  -- | Message from the backend
  ByteString ->
  IO ()
handleMessage withLog deployment websocketOptions connection payload =
  case WSS.parseMessage payload of
    Left err ->
      -- TODO: show the bytestring?
      withLog $ K.logLocM K.ErrorS $ K.ls $ "Failed to parse websocket payload: " <> err
    Right message ->
      handleCommand (WSS.command message)
  where
    -- TODO: cut down record access boilerplate

    -- Deployment details
    storePath = WSS.storePath deploymentDetails
    deploymentDetails = Agent.deploymentDetails deployment
    deploymentID = WSS.id (deploymentDetails :: WSS.DeploymentDetails)
    index = show $ WSS.index deploymentDetails
    profileName = Agent.profileName deployment
    agentToken = Agent.agentToken deployment
    agentInformation = Agent.agentInformation deployment

    -- WebSocket options
    host = WebSocket.host websocketOptions
    headers = WebSocket.headers websocketOptions

    handleCommand :: WSS.BackendCommand -> IO ()
    handleCommand (WSS.Deployment _) =
      withLog $ K.logLocM K.ErrorS "cachix-deployment should have never gotten a deployment command directly."
    handleCommand (WSS.AgentRegistered _) = do
      withLog $ K.logLocM K.InfoS $ K.ls $ "Deploying #" <> index <> ": " <> storePath

      let logPath = "/api/v1/deploy/log/" <> UUID.toText deploymentID
      logQueue <- atomically TMQueue.newTMQueue
      logThread <-
        Async.async $ streamLog withLog host logPath headers logQueue
      let logStream = Conduit.sinkTMQueue logQueue

      result <- Activate.withCacheArgs host agentInformation agentToken $ \cacheArgs -> do
        -- TODO: what if this fails
        closureSize <- fromRight Nothing <$> Activate.getClosureSize cacheArgs storePath
        startDeployment closureSize

        Exception.tryIO $ Activate.activate logStream profileName deploymentDetails cacheArgs

      let hasSucceeded = isRight result
      when hasSucceeded $
        logLine logStream "Successfully activated the deployment."

      endDeployment hasSucceeded

      -- Test network, run rollback script, and optionally trigger rollback

      -- case result of
      --   Left _ ->
      --     K.logLocM K.InfoS $ K.ls $ "Deploying #" <> index <> " failed."
      --   Right _ ->
      --     K.logLocM K.InfoS $ K.ls $ "Deployment #" <> index <> " finished"

      -- Cleanup

      -- Stop logging
      atomically (TMQueue.closeTMQueue logQueue)
      wait logThread

      WS.sendClose connection ("Closing." :: ByteString)
      where
        startDeployment closureSize = do
          now <- getCurrentTime
          sendMessage $
            WSS.DeploymentStarted
              { WSS.id = deploymentID,
                WSS.time = now,
                WSS.closureSize = closureSize
              }

        endDeployment :: Bool -> IO ()
        endDeployment hasSucceeded = do
          now <- getCurrentTime
          sendMessage $
            WSS.DeploymentFinished
              { WSS.id = deploymentID,
                WSS.time = now,
                WSS.hasSucceeded = hasSucceeded
              }

        sendMessage :: WSS.AgentCommand -> IO ()
        sendMessage cmd = do
          command <- createMessage cmd
          WSS.sendMessage connection command

        createMessage :: WSS.AgentCommand -> IO (WSS.Message WSS.AgentCommand)
        createMessage command = do
          uuid <- UUID.nextRandom
          return $
            WSS.Message
              { WSS.method = method,
                WSS.command = command,
                WSS.id = uuid,
                WSS.agent = Just $ WSS.id (agentInformation :: WSS.AgentInformation)
              }
          where
            -- TODO: move to WSS
            method = case command of
              WSS.DeploymentStarted {} -> "DeploymentStarted"
              WSS.DeploymentFinished {} -> "DeploymentFinished"

-- Logs

type LogStream = Conduit.ConduitT ByteString Conduit.Void IO ()

streamLog :: Log.WithLog -> Text -> Text -> RequestHeaders -> TMQueue.TMQueue ByteString -> IO ()
streamLog withLog host path headers queue = do
  -- retryAllWithLogging endlessRetryPolicy (WebSocket.logRetry withLog) $
  Wuss.runSecureClientWith (toS host) 443 (toS path) WS.defaultConnectionOptions headers $
    \conn -> do
      -- handle WebSocket.exitWithCloseRequest $
      Conduit.runConduit $
        Conduit.sourceTMQueue queue
          .| Conduit.linesUnboundedAscii
          -- TODO: prepend katip-like format to each line
          .| Conduit.mapM (\bs -> (withLog . K.logLocM K.DebugS . K.ls) bs >> return bs)
          .| sendLog conn

      WS.sendClose conn ("" :: ByteString)
      threadDelay (2 * 1000 * 1000)

logLine :: LogStream -> ByteString -> IO ()
logLine logStream msg = Conduit.connect (Conduit.yield $ "\n" <> msg <> "\n") logStream

sendLog :: WS.Connection -> LogStream
sendLog connection = Conduit.mapM_ $ \bs -> do
  now <- getCurrentTime
  WS.sendTextData connection $ Aeson.encode $ WSS.Log {WSS.line = toS bs, WSS.time = now}
