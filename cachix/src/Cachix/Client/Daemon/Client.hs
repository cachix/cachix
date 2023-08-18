module Cachix.Client.Daemon.Client (push, stop) where

import Cachix.Client.Config as Config
import Cachix.Client.Daemon.Listen (getSocketPath)
import Cachix.Client.Daemon.Types (ClientMessage (..), PushRequest (..))
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions (..))
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Data.Aeson as Aeson
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString.Lazy as Socket.LBS
import Protolude
import qualified System.Posix.IO as Posix

-- | Queue up push requests with the daemon
--
-- TODO: wait for the daemon to respond that it has received the request
push :: Env -> DaemonOptions -> BinaryCacheName -> [FilePath] -> IO ()
push Env {config, cachixoptions} daemonOptions cacheName storePaths = do
  sock <- connectToDaemon (daemonSocketPath daemonOptions)
  Socket.LBS.sendAll sock (Aeson.encode pushRequest)
  where
    pushRequest =
      ClientPushRequest $
        PushRequest (Config.authToken config) cacheName (Config.host cachixoptions) storePaths

-- | Tell the daemon to stop and wait for it gracefully exit
stop :: Env -> DaemonOptions -> IO ()
stop _env daemonOptions = do
  sock <- connectToDaemon (daemonSocketPath daemonOptions)
  Socket.LBS.sendAll sock (Aeson.encode ClientStop)

  -- Wait for the socket to close
  void $ Socket.LBS.recv sock 1

connectToDaemon :: Maybe FilePath -> IO Socket.Socket
connectToDaemon optionalSocketPath = do
  socketPath <- maybe getSocketPath pure optionalSocketPath
  sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  Socket.connect sock (Socket.SockAddrUnix socketPath)

  -- Network.Socket.accept sets the socket to non-blocking by default.
  Socket.withFdSocket sock $ \fd ->
    Posix.setFdOption (fromIntegral fd) Posix.NonBlockingRead False

  return sock
