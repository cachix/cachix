{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Agent where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.Config as Config
import Cachix.Client.URI (URI)
import qualified Cachix.Client.URI as URI
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.Deployment as Deployment
import qualified Cachix.Deploy.Lock as Lock
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Deploy.OptionsParser as CLI
import qualified Cachix.Deploy.StdinProcess as StdinProcess
import qualified Cachix.Deploy.Websocket as WebSocket
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Extra (once)
import qualified Control.Concurrent.MVar as MVar
import Control.Exception.Safe (onException, throwString)
import qualified Control.Exception.Safe as Safe
import qualified Control.Retry as Retry
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.String (String)
import qualified Katip as K
import Paths_cachix (getBinDir)
import Protolude hiding (onException, toS)
import Protolude.Conv
import qualified System.Directory as Directory
import System.Environment (getEnv, lookupEnv)
import qualified System.Posix.Files as Posix.Files
import qualified System.Posix.Process as Posix
import qualified System.Posix.Signals as Signals
import qualified System.Posix.Types as Posix
import qualified System.Posix.User as Posix.User
import qualified System.Timeout as Timeout

type ServiceWebSocket = WebSocket.WebSocket (WSS.Message WSS.AgentCommand) (WSS.Message WSS.BackendCommand)

data Agent = Agent
  { name :: Text,
    token :: Text,
    profileName :: Text,
    agentState :: IORef (Maybe WSS.AgentInformation),
    pid :: Posix.CPid,
    bootstrap :: Bool,
    host :: URI,
    logOptions :: Log.Options,
    withLog :: Log.WithLog,
    websocket :: ServiceWebSocket
  }

agentIdentifier :: Text -> Text
agentIdentifier agentName = unwords [agentName, toS versionNumber]

run :: Config.CachixOptions -> CLI.AgentOptions -> IO ()
run cachixOptions agentOptions =
  Log.withLog logOptions $ \withLog ->
    logExceptions withLog $
      withAgentLock agentOptions $ do
        checkUserOwnsHome

        -- TODO: show a more helpful error if the token is missing
        -- TODO: show a more helpful error when the token isn't valid
        -- TODO: wrap the token in a newtype or use servant's Token
        agentToken <- toS <$> getEnv "CACHIX_AGENT_TOKEN"
        agentState <- newIORef Nothing

        pid <- Posix.getProcessID

        let port = fromMaybe (URI.Port 80) $ (URI.getPortFor . URI.getScheme) host
        let websocketOptions =
              WebSocket.Options
                { WebSocket.host = basename,
                  WebSocket.port = port,
                  WebSocket.path = "/ws",
                  WebSocket.useSSL = URI.requiresSSL (URI.getScheme host),
                  WebSocket.headers = WebSocket.createHeaders agentName agentToken,
                  WebSocket.identifier = agentIdentifier agentName
                }

        websocket <- WebSocket.new withLog websocketOptions
        channel <- WebSocket.receive websocket
        shutdownWebsocket <- connectToService websocket

        installSignalHandlers shutdownWebsocket

        let agent =
              Agent
                { name = agentName,
                  token = agentToken,
                  profileName = profileName,
                  agentState = agentState,
                  pid = pid,
                  bootstrap = CLI.bootstrap agentOptions,
                  host = host,
                  logOptions = logOptions,
                  withLog = withLog,
                  websocket = websocket
                }

        WebSocket.readDataMessages channel $ \message ->
          handleCommand agent (WSS.command message)
  where
    host = Config.host cachixOptions
    basename = URI.getHostname host
    agentName = CLI.name agentOptions
    profileName = fromMaybe "system" (CLI.profile agentOptions)

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

lockFilename :: Text -> FilePath
lockFilename agentName = "agent-" <> toS agentName

-- | Acquire a lock for this agent. Skip this step if we're bootstrapping the agent.
withAgentLock :: CLI.AgentOptions -> IO () -> IO ()
withAgentLock CLI.AgentOptions {bootstrap = True} action = action
withAgentLock CLI.AgentOptions {name} action = tryToAcquireLock 0
  where
    tryToAcquireLock :: Int -> IO ()
    tryToAcquireLock attempts = do
      lock <- Lock.withTryLockAndPid (lockFilename name) action
      when (isNothing lock) $
        if attempts >= 5
          then throwIO (AgentAlreadyRunning name)
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
verifyBootstrapSuccess Agent {name, withLog} = do
  withLog . K.logLocM K.InfoS . K.ls $
    unwords ["Waiting for another agent to take over..."]

  eAgentPid <-
    Safe.tryIO $
      Retry.recoverAll
        (Retry.limitRetries 20 <> Retry.constantDelay 1000)
        (const waitForAgent)

  case eAgentPid of
    Right pid -> do
      withLog . K.logLocM K.InfoS . K.ls $
        unwords ["Found an active agent for", name, "with PID " <> show pid <> ".", "Exiting."]
      exitSuccess
    _ -> do
      withLog . K.logLocM K.InfoS . K.ls $
        unwords ["Cannot find an active agent for", name <> ".", "Waiting for more deployments."]
  where
    lockfile = lockFilename name

    -- The PID might be stale in rare cases. Only use this for diagnostics.
    waitForAgent :: IO Posix.CPid
    waitForAgent = do
      lock <- Lock.withTryLock lockfile (pure ())
      mpid <- Lock.readPidFile lockfile
      case (lock, mpid) of
        (Nothing, Just pid) -> pure pid
        _ -> throwString "No active agent found"

handleCommand :: Agent -> WSS.BackendCommand -> IO ()
handleCommand agent command =
  case command of
    WSS.AgentRegistered agentInformation -> registerAgent agent agentInformation
    WSS.Deployment deploymentDetails -> launchDeployment agent deploymentDetails

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
    throwIO $
      UserDoesNotOwnHome
        { userName = userName,
          sudoUser = sudoUser,
          home = home
        }

data AgentError
  = -- | An agent with the same name is already running.
    AgentAlreadyRunning Text
  | -- | No home directory.
    NoHomeFound
  | -- | Safeguard against creating root-owned files in user directories.
    -- This is an issue on macOS, where, by default, sudo does not reset $HOME.
    UserDoesNotOwnHome
      { userName :: String,
        sudoUser :: Maybe String,
        home :: FilePath
      }
  deriving (Show)

instance Exception AgentError where
  displayException = \case
    AgentAlreadyRunning agentName -> toS $ unwords ["The agent", agentName, "is already running."]
    NoHomeFound -> "Could not find the user’s home directory. Make sure to set the $HOME variable."
    UserDoesNotOwnHome {userName = userName, sudoUser = sudoUser, home = home} ->
      if isJust sudoUser
        then toS $ unlines [warningMessage, suggestSudoFlagH]
        else toS warningMessage
      where
        warningMessage = "The current user (" <> toS userName <> ") does not own the home directory (" <> toS home <> ")"
        suggestSudoFlagH = "Try running the agent with `sudo -H`."
