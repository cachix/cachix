module Cachix.Daemon.Types.EventLoop
  ( EventLoop (..),
    EventLoopError (..),
    ExitLatch,
  )
where

import Control.Concurrent.STM.TBMQueue (TBMQueue)
import Protolude

-- | An event loop that processes a queue of events.
data EventLoop event output = EventLoop
  { queue :: TBMQueue event,
    exitLatch :: ExitLatch output
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
