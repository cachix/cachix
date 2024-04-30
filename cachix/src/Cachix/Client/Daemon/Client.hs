module Cachix.Client.Daemon.Client (push, stop) where

import Cachix.Client.Daemon.Listen (getSocketPath)
import Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions (..))
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM.TBMQueue
import Control.Exception.Safe (catchAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.IORef
import Data.Time.Clock
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.BS
import qualified Network.Socket.ByteString.Lazy as Socket.LBS
import Protolude
import qualified System.Posix.IO as Posix

data DaemonException = Unresponsive deriving (Show)

instance Exception DaemonException

-- | Queue up push requests with the daemon
--
-- TODO: wait for the daemon to respond that it has received the request
push :: Env -> DaemonOptions -> [FilePath] -> IO ()
push _env daemonOptions storePaths =
  withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
    Socket.LBS.sendAll sock (Aeson.encode pushRequest)
  where
    pushRequest =
      Protocol.ClientPushRequest $
        PushRequest {storePaths = storePaths}

-- | Tell the daemon to stop and wait for it to gracefully exit
stop :: Env -> DaemonOptions -> IO ()
stop _env daemonOptions =
  withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
    putErrText "Setting up queues for daemon communication"
    let size = 100
    (rx, tx) :: (TBMQueue Protocol.DaemonMessage, TBMQueue Protocol.ClientMessage) <-
      atomically $ (,) <$> newTBMQueue size <*> newTBMQueue size

    mainThread <- myThreadId
    lastPongRef <- newIORef =<< getCurrentTime
    rxThread <- Async.async $ fix $ \loop -> do
      -- Wait for the socket to close
      bs <- Socket.BS.recv sock 4096 `catchAny` (\_ -> return BS.empty)
      putErrText "Received message from daemon"

      -- A zero-length response means that the daemon has closed the socket
      guard $ not $ BS.null bs

      case Aeson.eitherDecodeStrict bs of
        Left err -> do
          putErrText (toS err)
          throwTo mainThread Unresponsive
        Right msg -> do
          atomically $ writeTBMQueue rx msg
          loop

    txThread <- Async.async $ fix $ \loop -> do
      mmsg <- atomically $ readTBMQueue tx
      for_ mmsg $ \msg -> do
        Socket.LBS.sendAll sock (Aeson.encode msg)
        loop

    pingThread <- Async.async $ forever $ do
      timestamp <- getCurrentTime
      lastPong <- readIORef lastPongRef
      when (timestamp >= addUTCTime 30 lastPong) $ do
        putErrText "Daemon is unresponsive, killing"
        throwTo mainThread Unresponsive
      putErrText "Sending ping to daemon"
      atomically $ writeTBMQueue tx Protocol.ClientPing
      threadDelay (20 * 1000 * 1000)

    -- Request the daemon to stop
    putErrText "Sending client stop"
    atomically $ writeTBMQueue tx Protocol.ClientStop

    fix $ \loop -> do
      atomically (readTBMQueue rx) >>= \case
        Nothing -> return ()
        Just Protocol.DaemonPong -> do
          putErrText "Got back pong"
          writeIORef lastPongRef =<< getCurrentTime
          loop
        Just Protocol.DaemonBye -> do
          Async.cancel rxThread
          Async.cancel txThread
          Async.cancel pingThread
          return ()

withDaemonConn :: Maybe FilePath -> (Socket.Socket -> IO a) -> IO a
withDaemonConn optionalSocketPath f = do
  putErrText "Connecting to Cachix Daemon"
  socketPath <- maybe getSocketPath pure optionalSocketPath
  bracket (open socketPath) Socket.close f `onException` failedToConnectTo socketPath
  where
    open socketPath = do
      sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
      putErrText $ "Connecting to: " <> show socketPath
      Socket.connect sock (Socket.SockAddrUnix socketPath)

      putErrText "Setting up non-blocking socket"
      -- Network.Socket.accept sets the socket to non-blocking by default.
      Socket.withFdSocket sock $ \fd ->
        Posix.setFdOption (fromIntegral fd) Posix.NonBlockingRead False

      return sock

    failedToConnectTo :: FilePath -> IO ()
    failedToConnectTo socketPath = do
      putErrText "Failed to connect to Cachix Daemon"
      putErrText $ "Tried to connect to: " <> toS socketPath <> "\n"
