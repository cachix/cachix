module Cachix.Client.Daemon.Client (push, stop, stopAndWait) where

import Cachix.Client.Config as Config
import Cachix.Client.Daemon.Listen (getSocketPath)
import Cachix.Client.Daemon.Types (ClientMessage (..), DaemonMessage (..), PushRequest (..))
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions (..))
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Control.Concurrent.Async as Async
import Control.Exception.Safe (catchAny)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.BS
import qualified Network.Socket.ByteString.Lazy as Socket.LBS
import Protolude
import Servant.Auth.Client (getToken)
import qualified System.Posix.IO as Posix

-- | Queue up push requests with the daemon
--
-- TODO: wait for the daemon to respond that it has received the request
push :: Env -> DaemonOptions -> BinaryCacheName -> [FilePath] -> IO ()
push Env {config, cachixoptions} daemonOptions cacheName storePaths =
  withDaemonConn (daemonSocketPath daemonOptions) $ \sock -> do
    Socket.LBS.sendAll sock (Aeson.encode pushRequest)
    Socket.gracefulClose sock 5000
  where
    authToken =
      if BS.null . getToken $ Config.authToken config
        then Nothing
        else Just (Config.authToken config)

    pushRequest =
      ClientPushRequest $
        PushRequest
          { authToken = authToken,
            signingKey = Nothing,
            cacheName = cacheName,
            host = Config.host cachixoptions,
            storePaths = storePaths
          }

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
  bracket (open socketPath) Socket.close f
  where
    open socketPath = do
      sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
      Socket.connect sock (Socket.SockAddrUnix socketPath)

      -- Network.Socket.accept sets the socket to non-blocking by default.
      Socket.withFdSocket sock $ \fd ->
        Posix.setFdOption (fromIntegral fd) Posix.NonBlockingRead False

      return sock
