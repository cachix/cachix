module Cachix.Daemon.Types.EventLoop
  ( EventLoop (..),
    EventLoopError (..),
    ExitLatch,
    DaemonEvent (..),
  )
where

import qualified Cachix.Daemon.Protocol as Protocol
import Cachix.Daemon.Types.SocketStore (SocketId)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Network.Socket (Socket)
import Protolude

-- | An event loop that processes 'DaemonEvent's.
data EventLoop a = EventLoop
  { queue :: TBMQueue DaemonEvent,
    exitLatch :: ExitLatch a
  }

-- | An exit latch is a semaphore that signals the event loop to exit.
-- The exit code should be returned by the 'EventLoop'.
type ExitLatch a = MVar (Either EventLoopError a)

data EventLoopError
  = EventLoopClosed
  | EventLoopFull
  deriving stock (Show, Eq)

instance Exception EventLoopError where
  displayException = \case
    EventLoopClosed -> "The event loop is closed"
    EventLoopFull -> "The event loop is full"

-- | Daemon events that are handled by the 'EventLoop'.
data DaemonEvent
  = -- | Shut down the daemon gracefully.
    ShutdownGracefully
  | -- | Re-establish the daemon socket
    ReconnectSocket
  | -- | Add a new client socket connection.
    AddSocketClient Socket
  | -- | Remove an existing client socket connection. For example, after it is closed.
    RemoveSocketClient SocketId
  | -- | Handle a new message from a client.
    ReceivedMessage Protocol.ClientMessage
