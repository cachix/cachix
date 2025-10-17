module Cachix.Daemon.Types.TaskQueue
  ( TaskQueue (..),
    Prioritized (..),
  )
where

import Control.Concurrent.STM.TVar
import Data.Int (Int32)
import Data.PQueue.Max (MaxQueue)
import Protolude

-- | Internal wrapper that adds FIFO ordering within same-priority items
-- Items are ordered first by their task priority, then by insertion order (FIFO)
-- Uses circular sequence comparison to handle wraparound correctly
data Prioritized a = Prioritized
  { pTask :: !a,
    pSequence :: !Int32
  }
  deriving stock (Eq)

instance (Ord a) => Ord (Prioritized a) where
  compare p1 p2 =
    case compare (pTask p1) (pTask p2) of
      EQ -> compareSeq (pSequence p1) (pSequence p2)
      other -> other
    where
      -- Circular sequence comparison: older (smaller) sequences come first
      -- Uses signed arithmetic to handle wraparound (works for sequences up to 2^31 apart)
      compareSeq s1 s2 =
        let diff = s2 - s1
         in compare diff 0

-- | A priority queue that maintains TBMQueue-compatible API
-- The queue is unbounded and prioritizes items by their Ord instance
-- Items with the same priority are returned in FIFO order
-- Sequence counter wraps around after 2^32 inserts
newtype TaskQueue a = TaskQueue (TVar (MaxQueue (Prioritized a), Int32, Bool))
