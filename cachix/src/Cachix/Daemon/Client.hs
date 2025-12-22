module Cachix.Daemon.Client (push, stop) where

import Cachix.Client.Env as Env
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.OptionsParser (DaemonOptions (..), DaemonPushOptions (..))
import Cachix.Client.OptionsParser qualified as Options
import Cachix.Client.Retry qualified as Retry
import Cachix.Daemon.Listen (getSocketPath)
import Cachix.Daemon.Progress qualified as Daemon.Progress
import Cachix.Daemon.Protocol as Protocol
import Cachix.Daemon.Types.Error (DaemonError (..), HasExitCode (..))
import Cachix.Daemon.Types.PushEvent (PushEvent (..), PushEventMessage (..))
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM.TBMQueue
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.IORef
import Data.Text qualified as T
import Data.Time.Clock
import Hercules.CNix.Store qualified as Store
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.BS
import Network.Socket.ByteString.Lazy qualified as Socket.LBS
import Protolude
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice)
import System.IO.Error (isResourceVanishedError)

data SocketError
  = -- | The socket has been closed
    SocketClosed
  | -- | The socket has stopped responding to pings
    SocketStalled
  | -- | Failed to decode a message from the socket
    SocketDecodingError !Text
  deriving stock (Show)

instance Exception SocketError where
  displayException = \case
    SocketClosed -> "The socket has been closed"
    SocketStalled -> "The socket has stopped responding to pings"
    SocketDecodingError err -> "Failed to decode the message from socket: " <> toS err

-- | Run socket communication with ping/pong handling
withSocketComm :: Socket.Socket -> (STM (Maybe (Either SocketError Protocol.DaemonMessage)) -> (Protocol.ClientMessage -> STM ()) -> IO a) -> IO a
withSocketComm sock action = do
  let size = 1000
  (rx, tx) <- atomically $ (,) <$> newTBMQueue size <*> newTBMQueue size

  lastPongRef <- newIORef =<< getCurrentTime
  rxThread <- Async.async (handleIncoming lastPongRef rx sock)
  txThread <- Async.async (handleOutgoing tx sock)
  pingThread <- Async.async (runPingThread lastPongRef rx tx)

  let threads = [rxThread, txThread, pingThread]
  mapM_ Async.link threads

  finally (action (readTBMQueue rx) (writeTBMQueue tx)) (mapM_ Async.cancel threads)
  where
    runPingThread lastPongRef rx tx = go
      where
        go = do
          timestamp <- getCurrentTime
          lastPong <- readIORef lastPongRef

          if timestamp >= addUTCTime 20 lastPong
            then atomically $ writeTBMQueue rx (Left SocketStalled)
            else do
              atomically $ writeTBMQueue tx Protocol.ClientPing
              threadDelay (2 * 1000 * 1000)
              go

    handleOutgoing tx sock' = go
      where
        go = do
          mmsg <- atomically $ readTBMQueue tx
          case mmsg of
            Nothing -> return ()
            Just msg -> do
              Retry.retryAll $ const $ Socket.LBS.sendAll sock' $ Protocol.newMessage msg
              go

    handleIncoming lastPongRef rx sock' = go BS.empty
      where
        socketClosed = atomically $ writeTBMQueue rx (Left SocketClosed)

        go leftovers = do
          ebs <- liftIO $ try $ Socket.BS.recv sock' 4096

          case ebs of
            Left err | isResourceVanishedError err -> socketClosed
            Left _err -> socketClosed
            Right bs | BS.null bs -> socketClosed
            Right bs -> do
              let (rawMsgs, newLeftovers) = Protocol.splitMessages (BS.append leftovers bs)

              forM_ (map Aeson.eitherDecodeStrict rawMsgs) $ \emsg -> do
                case emsg of
                  Left err -> do
                    let terr = toS err
                    putErrText terr
                    atomically $ writeTBMQueue rx (Left (SocketDecodingError terr))
                  Right msg -> do
                    case msg of
                      Protocol.DaemonPong -> do
                        writeIORef lastPongRef =<< getCurrentTime
                        atomically $ writeTBMQueue rx (Right msg)
                      _ -> atomically $ writeTBMQueue rx (Right msg)

              go newLeftovers

-- | Queue up push requests with the daemon and wait for completion
push :: Env -> DaemonOptions -> DaemonPushOptions -> [FilePath] -> IO ()
push _env daemonOptions daemonPushOptions cliPaths = do
  hasStdin <- not <$> hIsTerminalDevice stdin
  inputStorePaths <-
    case (hasStdin, cliPaths) of
      (False, []) -> throwIO $ NoInput "You need to specify store paths either as stdin or as a command argument"
      (True, []) -> map T.unpack . T.words <$> getContents
      -- If we get both stdin and cli args, prefer cli args.
      -- This avoids hangs in cases where stdin is non-interactive but unused by caller.
      (_, paths) -> return paths

  storePaths <- Store.withStore $ \store -> do
    inputStorePaths' <- mapM (Store.followLinksToStorePath store . BS8.pack) inputStorePaths
    mapM (fmap (toS . BS8.unpack) . Store.storePathToPath store) inputStorePaths'

  withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
    let shouldWait = Options.shouldWait daemonPushOptions
    let pushRequest = Protocol.ClientPushRequest (PushRequest {storePaths = storePaths, subscribeToUpdates = shouldWait})

    Socket.LBS.sendAll sock $ Protocol.newMessage pushRequest
    unless shouldWait exitSuccess

    withSocketComm sock $ \receive _send -> do
      progressState <- Daemon.Progress.newProgressState
      failureRef <- newIORef False

      fix $ \loop -> do
        mmsg <- atomically receive
        case mmsg of
          Nothing -> return ()
          Just (Left err) -> do
            putErrText $ toS $ displayException err
            exitFailure
          Just (Right msg) -> do
            case msg of
              Protocol.DaemonPong -> loop
              Protocol.DaemonPushEvent pushEvent -> do
                Daemon.Progress.displayPushEvent progressState pushEvent
                case eventMessage pushEvent of
                  PushStorePathFailed _ _ -> do
                    writeIORef failureRef True
                    loop
                  PushFinished -> do
                    hasFailures <- readIORef failureRef
                    if hasFailures
                      then exitWith (toExitCode DaemonPushFailure)
                      else exitSuccess
                  _ -> loop
              Protocol.DaemonError err -> handleDaemonError err
              Protocol.DaemonExit exitStatus -> handleDaemonExit exitStatus
              Protocol.DaemonDiagnosticsResult _ -> loop

-- | Tell the daemon to stop and wait for it to gracefully exit
stop :: Env -> DaemonOptions -> IO ()
stop _env daemonOptions =
  withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
    withSocketComm sock $ \receive send -> do
      atomically $ send Protocol.ClientStop

      fix $ \loop -> do
        mmsg <- atomically receive
        case mmsg of
          Nothing -> return ()
          Just (Left err) -> do
            putErrText $ toS $ displayException err
            exitFailure
          Just (Right msg) ->
            case msg of
              Protocol.DaemonPong -> loop
              Protocol.DaemonError err -> handleDaemonError err
              Protocol.DaemonExit exitStatus -> handleDaemonExit exitStatus
              Protocol.DaemonPushEvent _ -> loop
              Protocol.DaemonDiagnosticsResult _ -> loop

withDaemonConn :: Maybe FilePath -> (Socket.Socket -> IO a) -> IO a
withDaemonConn optionalSocketPath f = do
  envSocketPath <- lookupEnv "CACHIX_DAEMON_SOCKET"
  socketPath <- maybe getSocketPath pure (envSocketPath <|> optionalSocketPath)
  bracket (open socketPath `onException` failedToConnectTo socketPath) Socket.close f
  where
    open socketPath = do
      sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
      Retry.retryAll $ const $ Socket.connect sock (Socket.SockAddrUnix socketPath)
      return sock

    failedToConnectTo :: FilePath -> IO ()
    failedToConnectTo socketPath = do
      putErrText "\nFailed to connect to Cachix Daemon"
      putErrText $ "Tried to connect to: " <> toS socketPath <> "\n"

handleDaemonExit :: Protocol.DaemonExitStatus -> IO a
handleDaemonExit exitStatus =
  case exitCode exitStatus of
    0 -> exitSuccess
    code -> do
      case exitMessage exitStatus of
        Just msg -> putErrText $ toS msg
        Nothing -> return ()
      exitWith (ExitFailure code)

handleDaemonError :: Protocol.DaemonErrorMessage -> IO a
handleDaemonError err =
  case err of
    Protocol.UnsupportedCommand errMsg -> do
      putErrText $ toS errMsg
      exitWith (ExitFailure 2)
