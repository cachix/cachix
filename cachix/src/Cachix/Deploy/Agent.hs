{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.Deploy.Agent where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Cachix.Client.Config as Config
import Cachix.Client.URI (URI)
import qualified Cachix.Client.URI as URI
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.Deployment as Deployment
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Deploy.OptionsParser as AgentOptions
import qualified Cachix.Deploy.StdinProcess as StdinProcess
import qualified Cachix.Deploy.Websocket as WebSocket
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Extra (once)
import qualified Control.Concurrent.MVar as MVar
import Control.Exception.Safe (handleAny, onException)
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
import qualified System.Posix.Signals as Signals
import qualified System.Posix.User as Posix.User

type ServiceWebSocket = WebSocket.WebSocket (WSS.Message WSS.AgentCommand) (WSS.Message WSS.BackendCommand)

data Agent = Agent
  { agentState :: IORef (Maybe WSS.AgentInformation),
    name :: Text,
    token :: Text,
    profileName :: Text,
    host :: URI,
    logOptions :: Log.Options,
    withLog :: Log.WithLog,
    websocket :: ServiceWebSocket
  }

agentIdentifier :: Text -> Text
agentIdentifier agentName = agentName <> " " <> toS versionNumber

run :: Config.CachixOptions -> AgentOptions.AgentOptions -> IO ()
run cachixOptions agentOpts =
  Log.withLog logOptions $ \withLog ->
    handleAny (logAndExitWithFailure withLog) $ do
      checkUserOwnsHome

      -- TODO: show a more helpful error if the token is missing
      -- TODO: show a more helpful error when the token isn't valid
      agentToken <- toS <$> getEnv "CACHIX_AGENT_TOKEN"
      agentState <- newIORef Nothing

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
              { agentState = agentState,
                name = agentName,
                token = agentToken,
                profileName = profileName,
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
    agentName = AgentOptions.name agentOpts
    profileName = fromMaybe "system" (AgentOptions.profile agentOpts)

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

registerAgent :: Agent -> WSS.AgentInformation -> K.KatipContextT IO ()
registerAgent Agent {agentState} agentInformation = do
  K.logLocM K.InfoS "Agent registered."
  liftIO $ atomicWriteIORef agentState (Just agentInformation)

launchDeployment :: Agent -> WSS.DeploymentDetails -> IO ()
launchDeployment Agent {agentState, name, token, profileName, host, logOptions} deploymentDetails = do
  agentRegistered <- readIORef agentState

  case agentRegistered of
    -- TODO: this is currently not possible, but relies on the backend
    -- to do the right thing. Can we improve the typing here?
    Nothing -> pure ()
    Just agentInformation -> do
      binDir <- toS <$> getBinDir
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

handleCommand :: Agent -> WSS.BackendCommand -> IO ()
handleCommand agent@Agent {withLog} command =
  case command of
    WSS.AgentRegistered agentInformation -> withLog $ registerAgent agent agentInformation
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
  = -- | No home directory.
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
  displayException NoHomeFound = "Could not find the user’s home directory. Make sure to set the $HOME variable."
  displayException UserDoesNotOwnHome {userName = userName, sudoUser = sudoUser, home = home} =
    if isJust sudoUser
      then toS $ unlines [warningMessage, suggestSudoFlagH]
      else toS warningMessage
    where
      warningMessage = "The current user (" <> toS userName <> ") does not own the home directory (" <> toS home <> ")"
      suggestSudoFlagH = "Try running the agent with `sudo -H`."
