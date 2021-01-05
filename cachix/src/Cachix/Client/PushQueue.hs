{- Implements a queue with the following properties:

- waits for queue to be fully pushed when exiting using ctrl-c (SIGINT)
- allows stopping the producer
- avoid duplicate pushing of the same store paths

To safetly exit on demand, signal SIGINT.
-}
module Cachix.Client.PushQueue
  ( pushQueue,
    Queue,
  )
where

import qualified Cachix.Client.Push as Push
import Control.Concurrent.Async
import Control.Concurrent.STM (TVar, modifyTVar', newTVarIO, readTVarIO)
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import qualified Data.Set as S
import Protolude
import qualified System.Posix.Signals as Signals

type StorePath = Text

type Queue = TBQueue.TBQueue StorePath

data WorkerState
  = WorkerState
      { inQueue :: Queue,
        inProgress :: TVar (S.Set StorePath)
      }

worker :: Push.PushCache IO () -> WorkerState -> IO ()
worker pushCache workerState = forever $ do
  storePath <- atomically $ TBQueue.readTBQueue $ inQueue workerState
  inprogress <- readTVarIO $ inProgress workerState
  -- TODO: there's a small window between checking membership and inserting new path
  unless (S.member storePath inprogress) $ bracket_ (start storePath) (stop storePath) $
    Push.pushSingleStorePath pushCache storePath
  where
    start storePath =
      atomically $ modifyTVar' (inProgress workerState) (S.insert storePath)
    stop storePath =
      atomically $ modifyTVar' (inProgress workerState) (S.delete storePath)

-- NOTE: producer is responsible for signaling SIGINT upon termination
pushQueue :: Int -> (Queue -> IO (IO ())) -> Push.PushCache IO () -> IO ()
pushQueue numWorkers mkProducer pushCache = do
  queue <- atomically $ TBQueue.newTBQueue 10000
  stopProducerCallback <- mkProducer queue
  progress <- newTVarIO S.empty
  let workerState = WorkerState queue progress
  consumer <- async $ replicateConcurrently_ numWorkers $ worker pushCache workerState
  void $ Signals.installHandler Signals.sigINT (Signals.CatchOnce (exitOnceQueueIsEmpty stopProducerCallback consumer workerState)) Nothing
  wait consumer

exitOnceQueueIsEmpty :: IO () -> Async () -> WorkerState -> IO ()
exitOnceQueueIsEmpty stopProducerCallback consumer workerState = do
  putText "Stopping producer of store paths and waiting for queue to empty ..."
  stopProducerCallback
  go
  where
    go = do
      queueLength <- atomically $ TBQueue.lengthTBQueue $ inQueue workerState
      inprogress <- readTVarIO $ inProgress workerState
      if queueLength == 0 && S.null inprogress
        then do
          putText "All done, exiting."
          uninterruptibleCancel consumer
        else do
          putText $ "Waiting to finish: " <> show (S.size inprogress) <> " in progress, " <> show queueLength <> " in queue"
          threadDelay (1000 * 1000)
          go
