{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Agent where

import Cachix.API.WebSocketSubprotocol qualified as WSS
import Cachix.Client.Config qualified as Config
import Cachix.Client.URI (URI)
import Cachix.Client.URI qualified as URI
import Cachix.Client.Version (versionNumber)
import Cachix.Deploy.Deployment qualified as Deployment
import Cachix.Deploy.Lock qualified as Lock
import Cachix.Deploy.Log qualified as Log
import Cachix.Deploy.OptionsParser qualified as CLI
import Cachix.Deploy.StdinProcess qualified as StdinProcess
import Cachix.Deploy.Websocket qualified as WebSocket
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.Extra (once)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception.Safe (onException)
import Control.Exception.Safe qualified as Safe
import Control.Retry qualified as Retry
import Data.Aeson qualified as Aeson
import Data.IORef
import Data.String (String)
import Katip qualified as K
import Paths_cachix (getBinDir)
import Protolude hiding (onException, toS, (<.>))
import Protolude.Conv
import System.Directory qualified as Directory
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((<.>), (</>))
import System.Posix.Files qualified as Posix.Files
import System.Posix.Process qualified as Posix
import System.Posix.Signals qualified as Signals
import System.Posix.Types qualified as Posix
import System.Posix.User qualified as Posix.User
import System.Timeout qualified as Timeout

type ServiceWebSocket = WebSocket.WebSocket (WSS.Message WSS.AgentCommand) (WSS.Message WSS.BackendCommand)

data Agent = Agent
  { name :: Text,
    token :: Text,
    profileName :: Text,
    bootstrap :: Bool,
    host :: URI,
    websocket :: ServiceWebSocket,
    agentState :: IORef (Maybe WSS.AgentInformation),
    logOptions :: Log.Options,
    withLog :: Log.WithLog,
    lockFile :: FilePath,
    pid :: Posix.CPid,
    pidFile :: FilePath
  }

mkAgent :: Log.WithLog -> Log.Options -> Maybe FilePath -> Config.CachixOptions -> CLI.AgentOptions -> Text -> IO Agent
mkAgent withLog logOptions mlockDirectory cachixOptions agentOptions agentToken = do
  agentState <- newIORef Nothing
  pid <- Posix.getProcessID
  lockDirectory <- maybe Lock.getLockDirectory return mlockDirectory

  let host = Config.host cachixOptions
      basename = URI.getHostname host

      agentName = CLI.name agentOptions
      profileName = fromMaybe "system" (CLI.profile agentOptions)

      port =
        host
          & URI.getScheme
          & URI.getPortFor
          & fromMaybe (URI.Port 80)

      websocketOptions =
        WebSocket.Options
          { WebSocket.host = basename,
            WebSocket.port = port,
            WebSocket.path = "/ws",
            WebSocket.useSSL = URI.requiresSSL (URI.getScheme host),
            WebSocket.headers = WebSocket.createHeaders agentName agentToken,
            WebSocket.identifier = agentIdentifier agentName
          }

  websocket <- WebSocket.new withLog websocketOptions

  let lockFilename = "agent-" <> toS agentName

  return $
    Agent
      { name = agentName,
        token = agentToken,
        profileName = profileName,
        bootstrap = CLI.bootstrap agentOptions,
        agentState = agentState,
        pid = pid,
        pidFile = lockDirectory </> lockFilename <.> Lock.pidExtension,
        lockFile = lockDirectory </> lockFilename <.> Lock.lockExtension,
        host = host,
        logOptions = logOptions,
        withLog = withLog,
        websocket = websocket
      }

agentIdentifier :: Text -> Text
agentIdentifier agentName = unwords [agentName, toS versionNumber]

run :: Config.CachixOptions -> CLI.AgentOptions -> IO ()
run cachixOptions agentOptions =
  Log.withLog logOptions $ \withLog ->
    logExceptions withLog $ do
      checkUserOwnsHome

      -- TODO: show a more helpful error if the token is missing
      -- TODO: show a more helpful error when the token isn't valid
      -- TODO: wrap the token in a newtype or use servant's Token
      agentToken <- toS <$> getEnv "CACHIX_AGENT_TOKEN"

      agent <- mkAgent withLog logOptions Nothing cachixOptions agentOptions agentToken

      withAgentLock agent $ do
        -- Connect to the backend
        channel <- WebSocket.receive (websocket agent)
        shutdownWebsocket <- connectToService (websocket agent)

        -- Shutdown the socket on sigint
        installSignalHandlers shutdownWebsocket

        WebSocket.readDataMessages channel $ \message ->
          handleCommand agent (WSS.command message)
  where
    verbosity =
      if Config.verbose cachixOptions
        then Log.Verbose
        else Log.Normal

    logOptions =
      Log.Options
        { verbosity = verbosity,
          namespace = "agent",
          environment = "production"
        }

handleCommand :: Agent -> WSS.BackendCommand -> IO ()
handleCommand agent command =
  case command of
    WSS.AgentRegistered agentInformation -> registerAgent agent agentInformation
    WSS.Deployment deploymentDetails -> launchDeployment agent deploymentDetails

logExceptions :: Log.WithLog -> IO () -> IO ()
logExceptions withLog action =
  action `catches` [agentHandler, exceptionHandler]
  where
    -- Pretty-print any errors thrown by the agent
    agentHandler =
      Handler $ \(e :: AgentError) -> do
        withLog . K.logLocM K.ErrorS . K.ls $ displayException e
        exitFailure

    -- Handle any unexcepted exceptions
    exceptionHandler = Handler $ \(e :: SomeException) -> do
      withLog . K.logLocM K.ErrorS . K.ls $
        unlines
          [ "The agent encountered an exception:",
            toS (displayException e)
          ]
      exitFailure

-- | Acquire a lock for this agent. Skip this step if we're bootstrapping the agent.
withAgentLock :: Agent -> IO () -> IO ()
withAgentLock agent action =
  if bootstrap agent
    then action
    else tryToAcquireLock 0
  where
    agentName = name agent

    tryToAcquireLock :: Int -> IO ()
    tryToAcquireLock attempts = do
      lock <- Lock.withTryLockAndPid (lockFile agent) (pidFile agent) action
      when (isNothing lock) $
        if attempts >= 5
          then throwIO (AgentAlreadyRunning agentName)
          else do
            threadDelay (3 * 1000 * 1000)
            tryToAcquireLock (attempts + 1)

installSignalHandlers :: IO () -> IO ()
installSignalHandlers shutdown =
  for_ [Signals.sigINT, Signals.sigTERM] $ \signal ->
    Signals.installHandler signal handler Nothing
  where
    handler = Signals.CatchOnce shutdown

registerAgent :: Agent -> WSS.AgentInformation -> IO ()
registerAgent Agent {agentState, withLog} agentInformation = do
  withLog $ K.logLocM K.InfoS "Agent registered."
  atomicWriteIORef agentState (Just agentInformation)

launchDeployment :: Agent -> WSS.DeploymentDetails -> IO ()
launchDeployment agent@Agent {..} deploymentDetails = do
  agentRegistered <- readIORef agentState

  case agentRegistered of
    -- TODO: the agent should either not exist before we register or
    -- we should re-register here as a precaution.
    Nothing -> pure ()
    Just agentInformation -> do
      binDir <- toS <$> getBinDir
      exitCode <-
        StdinProcess.readProcess (binDir <> "/.cachix-deployment") [] $
          toS . Aeson.encode $
            Deployment.Deployment
              { Deployment.agentName = name,
                Deployment.agentToken = token,
                Deployment.profileName = profileName,
                Deployment.host = host,
                Deployment.deploymentDetails = deploymentDetails,
                Deployment.agentInformation = agentInformation,
                Deployment.logOptions = logOptions
              }

      when
        (bootstrap && exitCode == ExitSuccess)
        (verifyBootstrapSuccess agent)

verifyBootstrapSuccess :: Agent -> IO ()
verifyBootstrapSuccess agent@(Agent {name, withLog}) = do
  withLog . K.logLocM K.InfoS . K.ls $
    unwords ["Waiting for another agent to take over..."]

  magentPid <- waitForAgent (Retry.limitRetries 60 <> Retry.constantDelay 1000) agent

  case magentPid of
    Just pid -> do
      withLog . K.logLocM K.InfoS . K.ls $
        unwords ["Found an active agent for", name, "with PID " <> show pid <> ".", "Exiting."]
      exitSuccess
    Nothing -> do
      withLog . K.logLocM K.InfoS . K.ls $
        unwords ["Cannot find an active agent for", name <> ".", "Waiting for more deployments."]

waitForAgent :: Retry.RetryPolicyM IO -> Agent -> IO (Maybe Posix.CPid)
waitForAgent retryPolicy agent = do
  Retry.retrying
    retryPolicy
    (const $ pure . isNothing)
    (const $ findActiveAgent agent)

-- The PID might be stale in rare cases. Only use this for diagnostics.
findActiveAgent :: Agent -> IO (Maybe Posix.CPid)
findActiveAgent Agent {pidFile, lockFile} = do
  Safe.handleAny (const $ pure Nothing) $ do
    lock <- Lock.withTryLock lockFile (pure ())
    -- The lock must be held by another process
    guard (isNothing lock)

    -- We should have a PID file
    mpid <- Lock.readPidFile pidFile
    guard (isJust mpid)

    return mpid

-- | Asynchronously open and maintain a websocket connection to the backend for
-- sending deployment progress updates.
connectToService :: ServiceWebSocket -> IO (IO ())
connectToService websocket = do
  close <- MVar.newEmptyMVar

  thread <- Async.async $
    WebSocket.runConnection websocket $ do
      WebSocket.handleJSONMessages websocket (MVar.readMVar close)

  -- Block until the initial connection is established
  void $ MVar.readMVar (WebSocket.connection websocket)

  once $ do
    void $ MVar.tryPutMVar close ()
    void $ Timeout.timeout (5 * 1000 * 1000) (Async.wait thread)

-- | Fetch the home directory and verify that the owner matches the current user.
-- Throws either 'NoHomeFound' or 'UserDoesNotOwnHome'.
checkUserOwnsHome :: IO ()
checkUserOwnsHome = do
  home <- Directory.getHomeDirectory `onException` throwIO NoHomeFound
  stat <- Posix.Files.getFileStatus home
  userId <- Posix.User.getEffectiveUserID

  when (userId /= Posix.Files.fileOwner stat) $ do
    userName <- Posix.User.userName <$> Posix.User.getUserEntryForID userId
    sudoUser <- lookupEnv "SUDO_USER"
    throwIO $ UserDoesNotOwnHome userName sudoUser home

data AgentError
  = -- | An agent with the same name is already running.
    AgentAlreadyRunning Text
  | -- | No home directory.
    NoHomeFound
  | -- | Safeguard against creating root-owned files in user directories.
    -- This is an issue on macOS, where, by default, sudo does not reset $HOME.
    UserDoesNotOwnHome
      -- | The current user name
      String
      -- | The sudo user name, if any
      (Maybe String)
      -- | The home directory
      FilePath
  deriving (Show)

instance Exception AgentError where
  displayException = \case
    AgentAlreadyRunning agentName -> toS $ unwords ["The agent", agentName, "is already running."]
    NoHomeFound -> "Could not find the user’s home directory. Make sure to set the $HOME variable."
    UserDoesNotOwnHome userName sudoUser home ->
      if isJust sudoUser
        then toS $ unlines [warningMessage, suggestSudoFlagH]
        else toS warningMessage
      where
        warningMessage = "The current user (" <> toS userName <> ") does not own the home directory (" <> toS home <> ")"
        suggestSudoFlagH = "Try running the agent with `sudo -H`."
