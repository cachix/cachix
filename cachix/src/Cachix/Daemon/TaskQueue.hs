module Cachix.Daemon.TaskQueue
  ( TaskQueue,
    newTaskQueue,
    writeTask,
    tryWriteTask,
    readTask,
    closeTaskQueue,
  )
where

import Cachix.Daemon.Types.TaskQueue (Prioritized (..), TaskQueue (..))
import Control.Concurrent.STM.TVar
import Data.PQueue.Max qualified as MaxQueue
import Protolude

-- | Create a new empty task queue
newTaskQueue :: STM (TaskQueue a)
newTaskQueue = TaskQueue <$> newTVar (MaxQueue.empty, 0, False)

-- | Write an item to the queue (always succeeds unless queue is closed)
writeTask :: (Ord a) => TaskQueue a -> a -> STM ()
writeTask (TaskQueue tvar) task = do
  (queue, seqNum, closed) <- readTVar tvar
  unless closed $ do
    let prioritized = Prioritized task seqNum
        nextSeq = seqNum + 1 -- Word32 wraps around naturally on overflow
    writeTVar tvar (MaxQueue.insert prioritized queue, nextSeq, closed)

-- | Try to write an item to the queue
-- Returns Just True if successful, Just False if closed, Nothing if would block (never happens)
tryWriteTask :: (Ord a) => TaskQueue a -> a -> STM (Maybe Bool)
tryWriteTask (TaskQueue tvar) task = do
  (queue, seqNum, closed) <- readTVar tvar
  if closed
    then return (Just False)
    else do
      let prioritized = Prioritized task seqNum
          nextSeq = seqNum + 1
      writeTVar tvar (MaxQueue.insert prioritized queue, nextSeq, closed)
      return (Just True)

-- | Read an item from the queue
-- Returns Nothing if queue is closed, retries if queue is empty but open
readTask :: (Ord a) => TaskQueue a -> STM (Maybe a)
readTask (TaskQueue tvar) = do
  (queue, seqNum, closed) <- readTVar tvar
  case MaxQueue.maxView queue of
    Nothing ->
      if closed
        then return Nothing
        else retry
    Just (prioritized, queue') -> do
      writeTVar tvar (queue', seqNum, closed)
      return (Just (pTask prioritized))

-- | Close the queue
closeTaskQueue :: TaskQueue a -> STM ()
closeTaskQueue (TaskQueue tvar) = modifyTVar' tvar $ \(queue, seqNum, _) -> (queue, seqNum, True)
