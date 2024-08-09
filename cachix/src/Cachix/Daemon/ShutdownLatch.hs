module Cachix.Daemon.ShutdownLatch
  ( ShutdownLatch,
    newShutdownLatch,
    waitForShutdown,
    initiateShutdown,
    isShuttingDown,
  )
where

import Control.Concurrent.MVar
import Protolude

-- | A latch to keep track of the shutdown process.
newtype ShutdownLatch = ShutdownLatch {unShutdownLatch :: MVar ()}

newShutdownLatch :: (MonadIO m) => m ShutdownLatch
newShutdownLatch = ShutdownLatch <$> liftIO newEmptyMVar

waitForShutdown :: (MonadIO m) => ShutdownLatch -> m ()
waitForShutdown = liftIO . readMVar . unShutdownLatch

initiateShutdown :: (MonadIO m) => ShutdownLatch -> m ()
initiateShutdown = void . liftIO . flip tryPutMVar () . unShutdownLatch

isShuttingDown :: (MonadIO m) => ShutdownLatch -> m Bool
isShuttingDown = liftIO . fmap not . isEmptyMVar . unShutdownLatch
