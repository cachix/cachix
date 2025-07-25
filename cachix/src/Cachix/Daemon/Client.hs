module Cachix.Daemon.Client (push, stop) where

import Cachix.Client.Env as Env
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.OptionsParser (DaemonOptions (..))
import Cachix.Client.Retry qualified as Retry
import Cachix.Daemon.Listen (getSocketPath)
import Cachix.Daemon.Protocol as Protocol
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

-- | Queue up push requests with the daemon
--
-- TODO: wait for the daemon to respond that it has received the request
push :: Env -> DaemonOptions -> [FilePath] -> IO ()
push _env daemonOptions cliPaths = do
  hasStdin <- not <$> hIsTerminalDevice stdin
  inputStorePaths <-
    case (hasStdin, cliPaths) of
      (False, []) -> throwIO $ NoInput "You need to specify store paths either as stdin or as a command argument"
      (True, []) -> map T.unpack . T.words <$> getContents
      -- If we get both stdin and cli args, prefer cli args.
      -- This avoids hangs in cases where stdin is non-interactive but unused by caller.
      (_, paths) -> return paths

  Store.withStore $ \store -> do
    inputStorePaths' <- mapM (Store.followLinksToStorePath store . BS8.pack) inputStorePaths
    inputStorePathsStr <- mapM (fmap (toS . BS8.unpack) . Store.storePathToPath store) inputStorePaths'

    withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
      let pushRequest =
            Protocol.ClientPushRequest $
              PushRequest {storePaths = inputStorePathsStr}

      Socket.LBS.sendAll sock $ Protocol.newMessage pushRequest

-- | Tell the daemon to stop and wait for it to gracefully exit
stop :: Env -> DaemonOptions -> IO ()
stop _env daemonOptions =
  withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
    let size = 100
    (rx, tx) <- atomically $ (,) <$> newTBMQueue size <*> newTBMQueue size

    rxThread <- Async.async (handleIncoming rx sock)
    txThread <- Async.async (handleOutgoing tx sock)

    lastPongRef <- newIORef =<< getCurrentTime
    pingThread <- Async.async (runPingThread lastPongRef rx tx)

    mapM_ Async.link [rxThread, txThread, pingThread]

    -- Request the daemon to stop
    atomically $ writeTBMQueue tx Protocol.ClientStop

    fix $ \loop -> do
      mmsg <- atomically (readTBMQueue rx)
      case mmsg of
        Nothing -> return ()
        Just (Left err) -> do
          putErrText $ toS $ displayException err
          exitFailure
        Just (Right msg) ->
          case msg of
            Protocol.DaemonPong -> do
              writeIORef lastPongRef =<< getCurrentTime
              loop
            Protocol.DaemonExit exitStatus ->
              case exitCode exitStatus of
                0 -> exitSuccess
                code -> exitWith (ExitFailure code)
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

    handleOutgoing tx sock = go
      where
        go = do
          mmsg <- atomically $ readTBMQueue tx
          case mmsg of
            Nothing -> return ()
            Just msg -> do
              Retry.retryAll $ const $ Socket.LBS.sendAll sock $ Protocol.newMessage msg
              go

    handleIncoming rx sock = go BS.empty
      where
        socketClosed = atomically $ writeTBMQueue rx (Left SocketClosed)

        go leftovers = do
          ebs <- liftIO $ try $ Socket.BS.recv sock 4096

          case ebs of
            Left err | isResourceVanishedError err -> socketClosed
            Left _err -> socketClosed
            -- If the socket returns 0 bytes, then it is closed
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
                    atomically $ writeTBMQueue rx (Right msg)

              go newLeftovers

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
