module Cachix.Client.Daemon.Push (push) where

import Cachix.Client.Config as Config
import Cachix.Client.Daemon.Listen (getSocketPath)
import Cachix.Client.Daemon.Types (PushRequest (..))
import Cachix.Client.Env as Env
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.BS
import qualified Network.Socket.ByteString.Lazy as Socket.LBS
import Protolude

-- | Queue up push requests with the daemon
push :: Env -> BinaryCacheName -> [FilePath] -> IO ()
push Env {config, cachixoptions} cacheName storePaths = do
  socketPath <- maybe getSocketPath pure Nothing
  sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  Socket.connect sock (Socket.SockAddrUnix socketPath)

  Socket.LBS.sendAll sock (Aeson.encode payload)

  forever $ do
    response <- Socket.BS.recv sock 4096
    BS.hPut stderr response
  where
    payload =
      PushRequest (Config.authToken config) cacheName (Config.host cachixoptions) storePaths
