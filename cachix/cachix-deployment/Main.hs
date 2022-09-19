{-# LANGUAGE DuplicateRecordFields #-}

module Main
  ( main,
  )
where

import Cachix.API.Error (escalateAs)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Deploy.Activate as Activate
import qualified Cachix.Deploy.Agent as Agent
import qualified Cachix.Deploy.Lock as Lock
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Deploy.Websocket as WebSocket
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM.TMQueue as TMQueue
import qualified Control.Exception.Safe as Safe
import qualified Data.Aeson as Aeson
import qualified Data.Conduit.TQueue as Conduit
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.IO.Encoding
import qualified Katip as K
import qualified Network.HTTP.Simple as HTTP
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

  deployment@Agent.Deployment
    { agentName,
      agentToken,
      profileName,
      host,
      logOptions,
      deploymentDetails
    } <-
    escalateAs (FatalError . toS) . Aeson.eitherDecode . toS =<< getContents

  let deploymentID = WSS.id (deploymentDetails :: WSS.DeploymentDetails)
  let logPath = "/api/v1/deploy/log/" <> UUID.toText deploymentID
  let headers = WebSocket.createHeaders agentName agentToken
  let websocketOptions =
        WebSocket.Options
          { WebSocket.host = host,
            WebSocket.path = "/ws-deployment",
            WebSocket.headers = headers,
            WebSocket.agentIdentifier = Agent.agentIdentifier agentName
          }

  Log.withLog logOptions $ \withLog ->
    void . Lock.withTryLock profileName $ do
      -- Open a connection to logging stream
      (logQueue, loggingThread) <- runLogStream withLog host logPath (WebSocket.headers websocketOptions)

      -- Open a connection to Cachix and block until it's ready.
      (service, serviceThread) <- connectToService withLog websocketOptions

      deploy withLog deployment service (Conduit.sinkTMQueue logQueue)
        `finally` do
          withLog $ K.logLocM K.DebugS $ K.ls ("Cleaning up websocket connections" :: Text)
          atomically $ TMQueue.closeTMQueue logQueue
          WebSocket.close service
          Async.waitBoth loggingThread serviceThread

-- | Open and maintain a websocket connection to the backend for sending deployment
-- progress updates.
connectToService ::
  Log.WithLog ->
  WebSocket.Options ->
  IO (Agent.ServiceWebSocket, Async.Async ())
connectToService withLog websocketOptions = do
  initialConnection <- MVar.newEmptyMVar

  thread <- Async.async $
    WebSocket.withConnection withLog websocketOptions $ \websocket -> do
      MVar.putMVar initialConnection websocket
      WebSocket.handleJSONMessages websocket (WebSocket.consumeIntoVoid websocket)

  -- Block until the connection has been established
  websocket <- MVar.readMVar initialConnection

  return (websocket, thread)

-- | Run the deployment commands
deploy ::
  -- | Logging context
  Log.WithLog ->
  -- | Deployment information passed from the agent
  Agent.Deployment ->
  Agent.ServiceWebSocket ->
  -- | Logging Websocket connection
  Log.LogStream ->
  IO ()
deploy withLog deployment service logStream = do
  withLog $ K.logLocM K.InfoS $ K.ls $ "Deploying #" <> deploymentIndex <> ": " <> storePath

  activationStatus <- Safe.tryAny $
    Activate.withCacheArgs host agentInformation agentToken $ \cacheArgs -> do
      startDeployment Nothing

      Activate.downloadStorePaths logStream deploymentDetails cacheArgs

      -- Read the closure size and report
      --
      -- TODO: query the remote store to get the size before downloading (and
      -- possibly running out of disk space)
      closureSize <- fromRight Nothing <$> Activate.getClosureSize cacheArgs storePath
      when (isJust closureSize) $ startDeployment closureSize

      rollbackAction <- Activate.activate logStream profileName (toS storePath)

      -- Run tests on the new deployment
      testResults <- Safe.tryAny $ do
        -- Run network test
        pong <- WebSocket.waitForPong 10 service
        when (isNothing pong) $ throwIO Activate.NetworkTestFailure

        for (WSS.rollbackScript deploymentDetails) $ \rollbackScript -> do
          Log.streamLine logStream "Running rollback script."
          rollbackScriptResult <- Safe.tryIO $ Activate.runShellWithExitCode logStream (toS rollbackScript) []
          case rollbackScriptResult of
            Right ExitSuccess -> pure ()
            Right (ExitFailure _) -> throwIO Activate.RollbackFailure
            Left e -> throwIO (Activate.RollbackScriptFailure e)

      -- Roll back if any of the tests have failed
      when (isLeft testResults) $
        case rollbackAction of
          Just rollback -> do
            Log.streamLine logStream "Deployment failed, rolling back ..."
            rollback
          Nothing ->
            Log.streamLine logStream "Skipping rollback as this is the first deployment."

  case activationStatus of
    Left e -> do
      Log.streamLine logStream $
        toS $
          unwords
            [ "Failed to activate the deployment.",
              toS $ displayException e
            ]
      withLog $ K.logLocM K.InfoS $ K.ls $ "Deploying #" <> deploymentIndex <> " failed."
    Right _ -> do
      Log.streamLine logStream "Successfully activated the deployment."
      withLog $ K.logLocM K.InfoS $ K.ls $ "Deployment #" <> deploymentIndex <> " finished"

  endDeployment (isRight activationStatus)
  where
    -- TODO: cut down record access boilerplate

    -- Deployment details

    storePath = WSS.storePath deploymentDetails
    deploymentDetails = Agent.deploymentDetails deployment
    deploymentID = WSS.id (deploymentDetails :: WSS.DeploymentDetails)
    deploymentIndex = show $ WSS.index deploymentDetails
    host = Agent.host deployment
    profileName = Agent.profileName deployment
    agentToken = Agent.agentToken deployment
    agentInformation = Agent.agentInformation deployment

    startDeployment :: Maybe Int64 -> IO ()
    startDeployment closureSize = do
      now <- getCurrentTime
      msg <-
        createMessage $
          WSS.DeploymentStarted
            { WSS.id = deploymentID,
              WSS.time = now,
              WSS.closureSize = closureSize
            }
      WebSocket.send service (WebSocket.DataMessage msg)

    endDeployment :: Bool -> IO ()
    endDeployment hasSucceeded = do
      now <- getCurrentTime
      msg <-
        createMessage $
          WSS.DeploymentFinished
            { WSS.id = deploymentID,
              WSS.time = now,
              WSS.hasSucceeded = hasSucceeded
            }
      WebSocket.send service (WebSocket.DataMessage msg)

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

-- Log

-- TODO: prepend katip-like format to each line
-- TODO: Re-use the WebSocket module here (without ping?)
runLogStream ::
  -- | Logging context
  Log.WithLog ->
  -- | Host
  Text ->
  -- | Path
  Text ->
  -- | HTTP headers
  HTTP.RequestHeaders ->
  -- | Returns a queue for writing messages and the thread handle
  IO (TMQueue.TMQueue ByteString, Async.Async ())
runLogStream withLog host path headers = do
  queue <- TMQueue.newTMQueueIO
  thread <- Async.async $
    WebSocket.reconnectWithLog withLog $
      Wuss.runSecureClientWith (toS host) 443 (toS path) WS.defaultConnectionOptions headers $ \connection ->
        Log.streamLog withLog connection queue
          `finally` WebSocket.waitForGracefulShutdown connection
  return (queue, thread)
