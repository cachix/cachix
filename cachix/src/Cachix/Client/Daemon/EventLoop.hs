module Cachix.Client.Daemon.EventLoop (new, send, run, exitLoopWith, EventLoop, DaemonEvent (..)) where

import Cachix.Client.Daemon.Types.EventLoop (DaemonEvent (..), EventLoop)
import Control.Concurrent.STM.TBMQueue (newTBMQueueIO, readTBMQueue, writeTBMQueue)
import Protolude

type ExitLatch = MVar ExitCode

new :: (MonadIO m) => m EventLoop
new = liftIO $ newTBMQueueIO 100

send :: (MonadIO m) => EventLoop -> DaemonEvent -> m ()
send eventloop = liftIO . atomically . writeTBMQueue eventloop

run :: (MonadIO m) => EventLoop -> ((ExitLatch, DaemonEvent) -> m ()) -> m ExitCode
run eventloop f = do
  exitLatch <- liftIO newEmptyMVar
  fix $ \loop -> do
    mevent <- liftIO $ atomically $ readTBMQueue eventloop
    case mevent of
      Just event -> f (exitLatch, event)
      Nothing -> void $ liftIO $ tryPutMVar exitLatch ExitSuccess

    liftIO (tryReadMVar exitLatch) >>= \case
      Just exitCode -> return exitCode
      Nothing -> loop

exitLoopWith :: (MonadIO m) => ExitCode -> ExitLatch -> m ()
exitLoopWith exitCode exitLatch = void $ liftIO $ tryPutMVar exitLatch exitCode
