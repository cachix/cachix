{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

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
import Control.Exception.Safe (handleAny, onException, throwString)
import qualified Control.Exception.Safe as Safe
import qualified Control.Retry as Retry
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.String (String)
import qualified Data.Text as Text
import qualified Katip as K
import Paths_cachix (getBinDir)
import Protolude hiding (onException, toS, (<.>))
import Protolude.Conv
import qualified System.Directory as Directory
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((<.>), (</>))
import qualified System.Posix.Files as Posix.Files
import qualified System.Posix.Process as Posix
import qualified System.Posix.Signals as Signals
import qualified System.Posix.Types as Posix
import qualified System.Posix.User as Posix.User
import qualified System.Process as Process

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
agentIdentifier agentName = agentName <> " " <> toS versionNumber

run :: Config.CachixOptions -> CLI.AgentOptions -> IO ()
run cachixOptions agentOptions =
  Log.withLog logOptions $ \withLog ->
    handleAny (logAndExitWithFailure withLog) $
      withLockedProfile agentOptions profileName $ do
        checkUserOwnsHome

        -- TODO: show a more helpful error if the token is missing
        -- TODO: show a more helpful error when the token isn't valid
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

        let signalSet = Signals.CatchOnce (shutdownWebsocket >>= Async.wait)
        void $ Signals.installHandler Signals.sigINT signalSet Nothing
        void $ Signals.installHandler Signals.sigTERM signalSet Nothing

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

    logAndExitWithFailure withLog e = do
      void $ withLog $ K.logLocM K.ErrorS $ K.ls (displayException e)
      exitFailure

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

lockFilename :: Text -> FilePath
lockFilename profileName = "agent-" <> toS profileName

-- | Acquire a lock on the profile for this agent. Skip this step if we're bootstrapping the agent.
withLockedProfile :: CLI.AgentOptions -> Text -> IO () -> IO ()
withLockedProfile CLI.AgentOptions {bootstrap = True} _ action = action
withLockedProfile _ profileName action = do
  lock <- Lock.withTryLock (lockFilename profileName) action
  case lock of
    Nothing -> throwIO (ProfileLocked profileName)
    _ -> pure ()

registerAgent :: Agent -> WSS.AgentInformation -> IO ()
registerAgent Agent {agentState, withLog} agentInformation = do
  withLog $ K.logLocM K.InfoS "Agent registered."
  atomicWriteIORef agentState (Just agentInformation)

launchDeployment :: Agent -> WSS.DeploymentDetails -> IO ()
launchDeployment agent@Agent {..} deploymentDetails = do
  agentRegistered <- readIORef agentState

  case agentRegistered of
    -- TODO: this is currently not possible, but relies on the backend
    -- to do the right thing. Can we improve the typing here?
    Nothing -> pure ()
    Just agentInformation -> do
      binDir <- toS <$> getBinDir
      exitCode <-
        StdinProcess.spawnProcess (binDir <> "/.cachix-deployment") [] $
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

      case exitCode of
        ExitSuccess
          | bootstrap ->
            verifyBootstrapSuccess agent
        _ -> pure ()

verifyBootstrapSuccess :: Agent -> IO ()
verifyBootstrapSuccess Agent {profileName, withLog} = do
  withLog $ K.logLocM K.InfoS $ K.ls $ unwords ["Waiting for another agent to take over", profileName, "..."]

  lockDirectory <- Lock.getLockDirectory
  eProcessName <-
    Safe.tryIO $
      Retry.recoverAll
        (Retry.limitRetries 30 <> Retry.constantDelay 1000)
        (const $ getProcessName lockDirectory)

  case eProcessName of
    Right (pid, "cachix") -> do
      withLog $ K.logLocM K.InfoS $ K.ls $ unwords ["Found an active agent for", profileName, "with PID", show pid]
      exitSuccess
    _ -> pure ()
  where
    getProcessName :: FilePath -> IO (Posix.CPid, Text)
    getProcessName lockDirectory = do
      mpid <- readMaybe <$> readFile (lockDirectory </> lockFilename profileName <.> "pid")
      case mpid of
        Nothing -> throwString "No PID"
        Just pid -> do
          processName <- Process.readProcess "ps" ["--quick-pid", show pid, "-o", "comm", "--no-headers"] []
          pure (pid, Text.strip $ toS processName)

handleCommand :: Agent -> WSS.BackendCommand -> IO ()
handleCommand agent command =
  case command of
    WSS.AgentRegistered agentInformation -> registerAgent agent agentInformation
    WSS.Deployment deploymentDetails -> launchDeployment agent deploymentDetails

-- | Asynchronously open and maintain a websocket connection to the backend for
-- sending deployment progress updates.
connectToService :: ServiceWebSocket -> IO (IO (Async ()))
connectToService websocket = do
  initialConnection <- MVar.newEmptyMVar
  close <- MVar.newEmptyMVar

  thread <- Async.async $
    WebSocket.runConnection websocket $ do
      MVar.putMVar initialConnection ()
      WebSocket.handleJSONMessages websocket (MVar.readMVar close)

  -- Block until the connection has been established
  void $ MVar.takeMVar initialConnection

  once (MVar.putMVar close () $> thread)

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

data Error
  = -- | The target profile is already locked by another agent.
    ProfileLocked Text
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

instance Exception Error where
  displayException = \case
    ProfileLocked profileName -> toS $ unwords ["The profile", "\"" <> profileName <> "\"", "is already being managed by another Cachix Agent"]
    NoHomeFound -> "Could not find the user’s home directory. Make sure to set the $HOME variable."
    UserDoesNotOwnHome {userName = userName, sudoUser = sudoUser, home = home} ->
      if isJust sudoUser
        then toS $ unlines [warningMessage, suggestSudoFlagH]
        else toS warningMessage
      where
        warningMessage = "The current user (" <> toS userName <> ") does not own the home directory (" <> toS home <> ")"
        suggestSudoFlagH = "Try running the agent with `sudo -H`."
