{-# LANGUAGE DuplicateRecordFields #-}

module Main
  ( main,
  )
where

import Cachix.API.Error (escalateAs)
import Cachix.API.WebSocketSubprotocol qualified as AgentInformation (AgentInformation (..))
import Cachix.API.WebSocketSubprotocol qualified as DeploymentDetails (DeploymentDetails (..))
import Cachix.API.WebSocketSubprotocol qualified as WSS
import Cachix.Client.URI qualified as URI
import Cachix.Deploy.Activate qualified as Activate
import Cachix.Deploy.Agent qualified as Agent
import Cachix.Deploy.Deployment (Deployment (..))
import Cachix.Deploy.Lock qualified as Lock
import Cachix.Deploy.Log qualified as Log
import Cachix.Deploy.Websocket qualified as WebSocket
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM.TMQueue qualified as TMQueue
import Control.Exception.Safe qualified as Safe
import Data.Aeson qualified as Aeson
import Data.Conduit.TQueue qualified as Conduit
import Data.Time.Clock (getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.IO.Encoding
import Katip qualified as K
import Network.WebSockets qualified as WS
import Protolude hiding (toS)
import Protolude.Conv
import System.IO (BufferMode (..), hSetBuffering)

lockFilenameFrom :: Text -> FilePath
lockFilenameFrom agentName = "deployment-" <> toS agentName

-- | Activate the new deployment.
--
-- If the target profile is already locked by another deployment, exit
-- immediately and rely on the backend to reschedule.
main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  deployment@Deployment
    { agentName,
      agentToken,
      host,
      logOptions,
      deploymentDetails
    } <-
    escalateAs (FatalError . toS) . Aeson.eitherDecode . toS =<< getContents

  let deploymentID = DeploymentDetails.id deploymentDetails
  let headers = WebSocket.createHeaders agentName agentToken
  let port = fromMaybe (URI.Port 80) (URI.getPortFor (URI.getScheme host))
  let logWebsocketOptions =
        WebSocket.Options
          { WebSocket.host = URI.getHostname host,
            WebSocket.port = port,
            WebSocket.path = "/api/v1/deploy/log/" <> UUID.toText deploymentID,
            WebSocket.useSSL = URI.requiresSSL (URI.getScheme host),
            WebSocket.headers = headers,
            WebSocket.identifier = Agent.agentIdentifier agentName
          }
  let serviceWebsocketOptions =
        WebSocket.Options
          { WebSocket.host = URI.getHostname host,
            WebSocket.port = port,
            WebSocket.path = "/ws-deployment",
            WebSocket.useSSL = URI.requiresSSL (URI.getScheme host),
            WebSocket.headers = headers,
            WebSocket.identifier = Agent.agentIdentifier agentName
          }

  lockFilePath <- Lock.newLockFilePath (lockFilenameFrom agentName)

  Log.withLog logOptions $ \withLog ->
    void . Lock.withTryLock lockFilePath $ do
      -- Open a connection to logging stream
      (logQueue, loggingThread) <- runLogStream withLog logWebsocketOptions

      -- Open a connection to Cachix and block until it's ready.
      service <- WebSocket.new withLog serviceWebsocketOptions
      shutdownService <- Agent.connectToService service

      deploy withLog deployment service (Conduit.sinkTMQueue logQueue)
        `finally` do
          withLog $ K.logLocM K.DebugS "Cleaning up websocket connections"
          atomically $ TMQueue.closeTMQueue logQueue
          shutdownService
          Async.wait loggingThread

-- | Run the deployment commands
deploy ::
  -- | Logging context
  Log.WithLog ->
  -- | Deployment information passed from the agent
  Deployment ->
  Agent.ServiceWebSocket ->
  -- | Logging Websocket connection
  Log.LogStream ->
  IO ()
deploy withLog Deployment {..} service logStream = do
  withLog $ K.logLocM K.InfoS $ K.ls $ "Deploying #" <> deploymentIndex <> ": " <> storePath

  activationStatus <- handleAsActivationStatus $
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
      testResults <- handleAsFailureReason $ do
        -- Run a basic network test against the backend
        pong <- WebSocket.waitForPong 10 service
        when (isNothing pong) $ throwIO Activate.NetworkTestFailure

        -- Run the optional rollback script
        for (WSS.rollbackScript deploymentDetails) $ \rollbackScript -> do
          Log.streamLine logStream "Running rollback script."
          rollbackScriptResult <- Safe.tryIO $ Activate.runShellWithExitCode logStream (toS rollbackScript) []
          case rollbackScriptResult of
            Right ExitSuccess -> pure ()
            Right (ExitFailure _) -> throwIO Activate.RollbackScriptExitFailure
            Left e -> throwIO (Activate.RollbackScriptUnexpectedError e)

      -- Roll back if any of the tests have failed
      case testResults of
        Right _ -> pure Activate.Success
        Left testErrors ->
          case rollbackAction of
            Just rollback -> do
              Log.streamLine logStream "Deployment failed, rolling back ..."
              rollback
              throwIO (Activate.Rollback testErrors)
            Nothing -> do
              Log.streamLine logStream "Skipping rollback as this is the first deployment."
              throwIO (Activate.Failure testErrors)

  case activationStatus of
    Activate.Failure e -> logDeploymentFailed e
    Activate.Rollback e -> logDeploymentFailed e
    Activate.Success -> do
      -- NOTE: the activate command uses this message to detect the end of the log
      Log.streamLine logStream "Successfully activated the deployment."
      withLog $ K.logLocM K.InfoS $ K.ls $ "Deployment #" <> deploymentIndex <> " finished."

  endDeployment activationStatus
  where
    storePath = DeploymentDetails.storePath deploymentDetails
    deploymentID = DeploymentDetails.id deploymentDetails
    deploymentIndex = show (DeploymentDetails.index deploymentDetails)

    handleAsActivationStatus :: IO Activate.Status -> IO Activate.Status
    handleAsActivationStatus action =
      action
        `Safe.catches` [ Safe.Handler (\(e :: Activate.Status) -> pure e),
                         Safe.Handler (\(e :: SomeException) -> pure $ Activate.Failure (Activate.UnexpectedError e))
                       ]

    handleAsFailureReason :: IO a -> IO (Either Activate.FailureReason a)
    handleAsFailureReason action =
      fmap Right action
        `Safe.catches` [ Safe.Handler (\(e :: Activate.FailureReason) -> pure (Left e)),
                         Safe.Handler (\(e :: SomeException) -> pure $ Left (Activate.UnexpectedError e))
                       ]

    logDeploymentFailed e = do
      Log.streamLine logStream $
        toS $
          unwords
            -- NOTE: the activate command uses this message to detect the end of the log
            [ "Failed to activate the deployment.",
              toS $ displayException e
            ]
      withLog $ K.logLocM K.InfoS $ K.ls $ "Deploying #" <> deploymentIndex <> " failed."

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

    endDeployment :: Activate.Status -> IO ()
    endDeployment status = do
      let hasSucceeded =
            case status of
              Activate.Success -> True
              _ -> False
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
            WSS.agent = Just $ AgentInformation.id agentInformation
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
  Log.WithLog ->
  WebSocket.Options ->
  -- | Returns a queue for writing messages and the thread handle
  IO (TMQueue.TMQueue ByteString, Async.Async ())
runLogStream withLog options = do
  queue <- TMQueue.newTMQueueIO
  thread <- Async.async $
    WebSocket.reconnectWithLog withLog $
      WebSocket.runClientWith options WS.defaultConnectionOptions $ \connection ->
        Log.streamLog withLog connection queue
          `finally` WebSocket.waitForGracefulShutdown connection
  return (queue, thread)
