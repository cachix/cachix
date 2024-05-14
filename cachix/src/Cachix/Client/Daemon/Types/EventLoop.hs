module Cachix.Client.Daemon.Types.EventLoop
  ( EventLoop,
    DaemonEvent (..),
  )
where

import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Types.SocketStore (SocketId)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Network.Socket (Socket)

type EventLoop = TBMQueue DaemonEvent

data DaemonEvent
  = ShutdownGracefully
  | ReconnectSocket
  | AddSocketClient Socket
  | RemoveSocketClient SocketId
  | ReceivedMessage Protocol.ClientMessage
