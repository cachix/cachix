-- high level interface for websocket clients
module Cachix.Deploy.Websocket where

import Cachix.API.WebSocketSubprotocol (AgentInformation)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import Cachix.Client.Retry
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.Log as Log
import qualified Cachix.Deploy.WebsocketPong as WebsocketPong
import Control.Exception.Safe (catchAny, onException)
import Control.Retry (RetryStatus (..))
import qualified Control.Retry as Retry
import Data.Aeson (FromJSON, ToJSON)
import Data.IORef
import Data.String (String)
import qualified Katip as K
import Network.HTTP.Types (Header)
import qualified Network.WebSockets as WS
import Protolude hiding (catch, onException, toS)
import Protolude.Conv
import qualified System.Directory as Directory
import System.Environment (getEnv, lookupEnv)
import qualified System.Info
import qualified System.Posix.Files as Posix.Files
import qualified System.Posix.User as Posix.User
import qualified Wuss

type AgentState = IORef (Maybe WSS.AgentInformation)

data Options = Options
  { host :: Text,
    path :: Text,
    name :: Text,
    profile :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Input = Input
  { deploymentDetails :: WSS.DeploymentDetails,
    logOptions :: Log.Options,
    websocketOptions :: Options
  }
  deriving (Show, Generic, ToJSON, FromJSON)

system :: String
system = System.Info.arch <> "-" <> System.Info.os

runForever :: K.LogEnv -> Options -> (ByteString -> Log.WithLog -> WS.Connection -> AgentState -> ByteString -> IO ()) -> IO ()
runForever logEnv options cmd = do
  -- TODO: do we need the context part, or is KatipT enough for us?
  let withLog = K.runKatipContextT logEnv () "agent"

  checkUserOwnsHome `catchAny` \e -> do
    withLog $ K.logLocM K.ErrorS $ K.ls (displayException e)
    exitFailure

  -- TODO: error if token is missing
  agentToken <- getEnv "CACHIX_AGENT_TOKEN"
  agentState <- newIORef Nothing

  pongState <- WebsocketPong.newState
  mainThreadID <- myThreadId

  let pingHandler = do
        last <- WebsocketPong.secondsSinceLastPong pongState
        withLog $ K.logLocM K.DebugS $ K.ls $ "Sending WebSocket keep-alive ping, last pong was " <> (show last :: Text) <> " seconds ago"
        WebsocketPong.pingHandler pongState mainThreadID pongTimeout
      connectionOptions = WebsocketPong.installPongHandler pongState WS.defaultConnectionOptions

  -- TODO: use exponential retry with reset: https://github.com/Soostone/retry/issues/25
  -- retryWithLogging endlessConstantRetryPolicy (logRetry withLog) $ do
  withLog $ K.logLocM K.InfoS $ K.ls ("Agent " <> agentIdentifier <> " connecting to " <> toS (host options) <> toS (path options))

  -- refresh pong state in case we're reconnecting
  WebsocketPong.pongHandler pongState

  -- TODO: https://github.com/jaspervdj/websockets/issues/229
  Wuss.runSecureClientWith (toS $ host options) 443 (toS $ path options) connectionOptions (headers options (toS agentToken)) $ \connection -> do
    withLog $ K.logLocM K.InfoS "Connected to Cachix Deploy service"
    WS.withPingThread connection pingEvery pingHandler $
      WSS.recieveDataConcurrently
        connection
        (\message -> cmd message withLog connection agentState (toS agentToken))
  where
    agentIdentifier = name options <> " " <> toS versionNumber
    pingEvery = 30
    pongTimeout = pingEvery * 2

exitWithCloseRequest :: WS.ConnectionException -> IO ()
exitWithCloseRequest (WS.CloseRequest _ _) = return ()
exitWithCloseRequest e = throwIO e

headers :: Options -> ByteString -> [Header]
headers options agentToken =
  [ ("Authorization", "Bearer " <> toS agentToken),
    ("name", toS (name options)),
    ("version", toS versionNumber),
    ("system", toS system)
  ]

-- TODO: log the exception
logRetry :: Log.WithLog -> Bool -> SomeException -> RetryStatus -> IO ()
logRetry withLog _ exception retryStatus =
  withLog $ K.logLocM K.ErrorS $ K.ls $ "Retrying in " <> delay (rsPreviousDelay retryStatus) <> " due to an exception: " <> displayException exception
  where
    delay :: Maybe Int -> String
    delay Nothing = "0 seconds"
    delay (Just s) = show (floor (fromIntegral s / 1000 / 1000)) <> " seconds"

-- TODO: Move to the agent. If the websocket is going to parse messages then it
-- can’t also swallow errors like this.
parseMessage :: K.KatipContext m => FromJSON cmd => ByteString -> (WSS.Message cmd -> m ()) -> m ()
parseMessage payload m = do
  case WSS.parseMessage payload of
    Left err ->
      -- TODO: show the bytestring?
      K.logLocM K.ErrorS $ K.ls $ "Failed to parse websocket payload: " <> err
    Right message ->
      m message

-- commands

registerAgent :: AgentState -> AgentInformation -> K.KatipContextT IO ()
registerAgent agentState agentInformation = do
  K.logLocM K.InfoS "Agent registered."
  liftIO $ atomicWriteIORef agentState (Just agentInformation)

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
