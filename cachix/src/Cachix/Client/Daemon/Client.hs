module Cachix.Client.Daemon.Client (push, stop, stopAndWait) where

import Cachix.Client.Daemon.Listen (getSocketPath)
import Cachix.Client.Daemon.Types (ClientMessage (..), DaemonMessage (..), PushRequest (..))
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions (..))
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (catchAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.BS
import qualified Network.Socket.ByteString.Lazy as Socket.LBS
import Protolude
import qualified System.Posix.IO as Posix

-- | Queue up push requests with the daemon
--
-- TODO: wait for the daemon to respond that it has received the request
push :: Env -> DaemonOptions -> [FilePath] -> IO ()
push _env daemonOptions storePaths =
  withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
    Socket.LBS.sendAll sock (Aeson.encode pushRequest)
    Socket.gracefulClose sock 5000
  where
    pushRequest =
      ClientPushRequest $
        PushRequest {storePaths = storePaths}

-- | Tell the daemon to stop and wait for it to gracefully exit
stop :: Env -> DaemonOptions -> IO ()
stop _env daemonOptions =
  withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
    Socket.LBS.sendAll sock (Aeson.encode ClientStop)

stopAndWait :: Env -> DaemonOptions -> IO ()
stopAndWait _env daemonOptions =
  withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
    Async.concurrently_ (waitForResponse sock) $
      Socket.LBS.sendAll sock (Aeson.encode ClientStop)
  where
    waitForResponse sock = do
      -- Wait for the socket to close
      bs <- Socket.BS.recv sock 4096 `catchAny` (\_ -> return BS.empty)

      -- A zero-length response means that the daemon has closed the socket
      guard $ not $ BS.null bs

      case Aeson.eitherDecodeStrict bs of
        Left err -> putErrText (toS err)
        Right DaemonBye -> return ()

withDaemonConn :: Maybe FilePath -> (Socket.Socket -> IO a) -> IO a
withDaemonConn optionalSocketPath f = do
  socketPath <- maybe getSocketPath pure optionalSocketPath
  bracket (open socketPath) Socket.close f `onException` failedToConnectTo socketPath
  where
    open socketPath = do
      sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
      Socket.connect sock (Socket.SockAddrUnix socketPath)

      -- Network.Socket.accept sets the socket to non-blocking by default.
      Socket.withFdSocket sock $ \fd ->
        Posix.setFdOption (fromIntegral fd) Posix.NonBlockingRead False

      return sock

    failedToConnectTo :: FilePath -> IO ()
    failedToConnectTo socketPath = do
      putErrText "Failed to connect to Cachix Daemon"
      putErrText $ "Tried to connect to: " <> toS socketPath <> "\n"
