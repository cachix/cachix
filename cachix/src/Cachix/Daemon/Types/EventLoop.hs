module Cachix.Daemon.Types.EventLoop
  ( EventLoop (..),
    EventLoopError (..),
  )
where

import Cachix.Daemon.ShutdownLatch (ShutdownLatch)
import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Protolude

-- | An event loop that processes a queue of events.
data EventLoop event output = EventLoop
  { queue :: TBMQueue event,
    -- | Latch for signaling shutdown and returning results
    shutdownLatch :: ShutdownLatch EventLoopError output
  }

data EventLoopError
  = EventLoopClosed
  | EventLoopFull
  deriving stock (Show, Eq)

instance Exception EventLoopError where
  displayException = \case
    EventLoopClosed -> "The event loop is closed"
    EventLoopFull -> "The event loop is full"
