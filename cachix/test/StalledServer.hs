module StalledServer (withStalledServer) where

import Network.Socket qualified as Socket
import Protolude

-- Accept a connection and keep it open without sending any data. The action
-- receives the port and an action that waits until the client has connected.
withStalledServer :: (Int -> IO () -> IO a) -> IO a
withStalledServer action =
  bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \server -> do
    Socket.bind server (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    Socket.listen server 1
    address <- Socket.getSocketName server
    port <- case address of
      Socket.SockAddrInet port _ -> pure (fromIntegral port)
      _ -> panic "Expected an IPv4 listener"
    connected <- newEmptyMVar
    withAsync
      ( bracket (fst <$> Socket.accept server) Socket.close $ \_ -> do
          putMVar connected ()
          forever $ threadDelay 1_000_000
      )
      (\_ -> action port (takeMVar connected))
