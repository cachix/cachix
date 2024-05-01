module Cachix.Client.Daemon.Client (push, stop) where

import Cachix.Client.Daemon.Listen (getSocketPath)
import Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions (..))
import qualified Cachix.Client.Retry as Retry
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM.TBMQueue
import Control.Exception.Safe (catchAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as Lazy.Char8
import Data.IORef
import Data.Time.Clock
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.BS
import qualified Network.Socket.ByteString.Lazy as Socket.LBS
import Protolude

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
    SocketDecodingError err -> "Failed to decode message from socket: " <> toS err

-- | Queue up push requests with the daemon
--
-- TODO: wait for the daemon to respond that it has received the request
push :: Env -> DaemonOptions -> [FilePath] -> IO ()
push _env daemonOptions storePaths =
  withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
    Socket.LBS.sendAll sock $ Aeson.encode pushRequest `Lazy.Char8.snoc` '\n'
  where
    pushRequest =
      Protocol.ClientPushRequest $
        PushRequest {storePaths = storePaths}

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

    -- mapM_ Async.link [rxThread, txThread, pingThread]

    -- Request the daemon to stop
    atomically $ writeTBMQueue tx Protocol.ClientStop

    fix $ \loop -> do
      mmsg <- atomically (readTBMQueue rx)
      case mmsg of
        Nothing -> return ()
        Just (Left err) -> putErrText $ toS $ displayException err
        Just (Right msg) ->
          case msg of
            Protocol.DaemonPong -> do
              writeIORef lastPongRef =<< getCurrentTime
              loop
            Protocol.DaemonBye -> exitSuccess
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
              Retry.retryAll $ const $ Socket.LBS.sendAll sock $ Aeson.encode msg `Lazy.Char8.snoc` '\n'
              go

    handleIncoming rx sock = go
      where
        go = do
          -- Wait for the socket to close
          bs <- Socket.BS.recv sock 4096 `catchAny` (\_ -> return BS.empty)

          -- A zero-length response means that the daemon has closed the socket
          if BS.null bs
            then atomically $ writeTBMQueue rx (Left SocketClosed)
            else case Aeson.eitherDecodeStrict bs of
              Left err -> do
                let terr = toS err
                putErrText terr
                atomically $ writeTBMQueue rx (Left (SocketDecodingError terr))
              Right msg -> do
                atomically $ writeTBMQueue rx (Right msg)
                go

withDaemonConn :: Maybe FilePath -> (Socket.Socket -> IO a) -> IO a
withDaemonConn optionalSocketPath f = do
  socketPath <- maybe getSocketPath pure optionalSocketPath
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
