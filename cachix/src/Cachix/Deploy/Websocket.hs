{-# LANGUAGE RecordWildCards #-}

-- high level interface for websocket clients
module Cachix.Deploy.Websocket where

import Cachix.API.WebSocketSubprotocol (AgentInformation)
import qualified Cachix.API.WebSocketSubprotocol as WSS
import Cachix.Client.Retry
import Cachix.Client.Version (versionNumber)
import qualified Cachix.Deploy.WebsocketPong as WebsocketPong
import Control.Exception.Safe (catchAny, onException)
import Control.Retry (RetryStatus (..))
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
import qualified System.Posix.Files as Posix.Files
import qualified System.Posix.User as Posix.User
import qualified Wuss

type AgentState = IORef (Maybe WSS.AgentInformation)

data Options = Options
  { host :: Text,
    path :: Text,
    name :: Text,
    isVerbose :: Bool,
    profile :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON)

data Input = Input
  { deploymentDetails :: WSS.DeploymentDetails,
    websocketOptions :: Options
  }
  deriving (Show, Generic, ToJSON, FromJSON)

runForever :: Options -> (ByteString -> (K.KatipContextT IO () -> IO ()) -> WS.Connection -> AgentState -> ByteString -> K.KatipContextT IO ()) -> IO ()
runForever options cmd = withKatip (isVerbose options) $ \logEnv -> do
  let runKatip = K.runKatipContextT logEnv () "agent"

  checkUserOwnsHome `catchAny` \e -> do
    runKatip $ K.logLocM K.ErrorS $ K.ls (displayException e)
    exitFailure

  -- TODO: error if token is missing
  agentToken <- getEnv "CACHIX_AGENT_TOKEN"
  agentState <- newIORef Nothing

  pongState <- WebsocketPong.newState
  mainThreadID <- myThreadId

  let pingHandler = do
        last <- WebsocketPong.secondsSinceLastPong pongState
        runKatip $ K.logLocM K.DebugS $ K.ls $ "Sending WebSocket keep-alive ping, last pong was " <> (show last :: Text) <> " seconds ago"
        WebsocketPong.pingHandler pongState mainThreadID pongTimeout
      connectionOptions = WebsocketPong.installPongHandler pongState WS.defaultConnectionOptions
  runKatip $
    -- TODO: use exponential retry with reset: https://github.com/Soostone/retry/issues/25
    retryAllWithLogging endlessConstantRetryPolicy (logger runKatip) $
      do
        K.logLocM K.InfoS $ K.ls ("Agent " <> agentIdentifier <> " connecting to " <> toS (host options) <> toS (path options))
        liftIO $ do
          -- refresh pong state in case we're reconnecting
          WebsocketPong.pongHandler pongState
          -- TODO: https://github.com/jaspervdj/websockets/issues/229
          Wuss.runSecureClientWith (toS $ host options) 443 (toS $ path options) connectionOptions (headers options (toS agentToken)) $ \connection -> runKatip $ do
            K.logLocM K.InfoS "Connected to Cachix Deploy service"
            liftIO $
              WS.withPingThread connection pingEvery pingHandler $
                do
                  WSS.recieveDataConcurrently
                    connection
                    (\message -> runKatip (cmd message runKatip connection agentState (toS agentToken)))
  where
    agentIdentifier = name options <> " " <> toS versionNumber
    pingEvery = 30
    pongTimeout = pingEvery * 2

headers :: Options -> ByteString -> [Header]
headers options agentToken =
  [ ("Authorization", "Bearer " <> toS agentToken),
    ("name", toS (name options)),
    ("version", toS versionNumber)
  ]

-- TODO: log the exception
logger runKatip _ exception retryStatus =
  runKatip $
    K.logLocM K.ErrorS $ K.ls $ "Retrying in " <> delay (rsPreviousDelay retryStatus) <> " due to an exception: " <> displayException exception
  where
    delay :: Maybe Int -> String
    delay Nothing = "0 seconds"
    delay (Just s) = show (floor (fromIntegral s / 1000 / 1000)) <> " seconds"

withKatip :: Bool -> (K.LogEnv -> IO a) -> IO a
withKatip isVerbose =
  bracket createLogEnv K.closeScribes
  where
    permit = if isVerbose then K.DebugS else K.InfoS
    createLogEnv = do
      logEnv <- K.initLogEnv "agent" "production"
      stdoutScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem permit) K.V2
      K.registerScribe "stdout" stdoutScribe K.defaultScribeSettings logEnv

parseMessage :: FromJSON cmd => ByteString -> (WSS.Message cmd -> K.KatipContextT IO ()) -> K.KatipContextT IO ()
parseMessage payload m = do
  case WSS.parseMessage payload of
    (Left err) ->
      -- TODO: show the bytestring?
      K.logLocM K.ErrorS $ K.ls $ "Failed to parse websocket payload: " <> err
    (Right message) ->
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
    throwIO $ UserDoesNotOwnHome {..}

data Error
  = -- | No home directory.
    NoHomeFound
  | -- | Safeguard against creating root-owned files in user directories.
    -- This is an issue on macOS, where, by default, sudo does not reset $HOME.
    UserDoesNotOwnHome {userName :: String, sudoUser :: Maybe String, home :: FilePath}
  deriving (Show)

instance Exception Error where
  displayException NoHomeFound = "Could not find the user’s home directory. Make sure to set the $HOME variable."
  displayException UserDoesNotOwnHome {..} =
    if isJust sudoUser
      then toS $ unlines [warningMessage, suggestSudoFlagH]
      else toS warningMessage
    where
      warningMessage = "The current user (" <> toS userName <> ") does not own the home directory (" <> toS home <> ")"
      suggestSudoFlagH = "Try running the agent with `sudo -H`."
