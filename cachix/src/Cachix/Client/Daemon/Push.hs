module Cachix.Client.Daemon.Push (push) where

import Cachix.Client.Config as Config
import Cachix.Client.Daemon.Listen (getSocketPath)
import Cachix.Client.Daemon.Types (PushRequest (..))
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions (..))
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Data.Aeson as Aeson
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString.Lazy as Socket.LBS
import Protolude

-- | Queue up push requests with the daemon
--
-- TODO: wait for the daemon to response that it has received the request
push :: Env -> DaemonOptions -> BinaryCacheName -> [FilePath] -> IO ()
push Env {config, cachixoptions} daemonOptions cacheName storePaths = do
  socketPath <- maybe getSocketPath pure (daemonSocketPath daemonOptions)
  sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  Socket.connect sock (Socket.SockAddrUnix socketPath)

  Socket.LBS.sendAll sock (Aeson.encode payload)
  where
    payload =
      PushRequest (Config.authToken config) cacheName (Config.host cachixoptions) storePaths
