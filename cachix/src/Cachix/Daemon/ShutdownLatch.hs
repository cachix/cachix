module Cachix.Daemon.ShutdownLatch
  ( ShutdownLatch,
    newShutdownLatch,
    waitForShutdown,
    initiateShutdown,
    initiateShutdownWithResult,
    getResult,
    isShuttingDown,
    -- STM operations
    isShuttingDownSTM,
    initiateShutdownSTM,
    initiateShutdownWithResultSTM,
    getResultSTM,
    waitForShutdownSTM,
  )
where

import Control.Concurrent.STM
import Protolude

-- | A latch to keep track of the shutdown process.
-- A shutdown latch holds a result value (Either e a) when shutdown is initiated.
-- Nothing means that shutdown has not been requested yet.
newtype ShutdownLatch e a = ShutdownLatch {unShutdownLatch :: TVar (Maybe (Either e a))}

-- | Create a new shutdown latch
newShutdownLatch :: (MonadIO m) => m (ShutdownLatch e a)
newShutdownLatch = ShutdownLatch <$> liftIO (newTVarIO Nothing)

-- | Block until shutdown is requested and return the result
waitForShutdown :: (MonadIO m) => ShutdownLatch e a -> m (Either e a)
waitForShutdown latch = liftIO $ atomically $ waitForShutdownSTM latch

-- | Signal shutdown with a "success" result
initiateShutdown :: (MonadIO m) => a -> ShutdownLatch e a -> m ()
initiateShutdown val latch = liftIO $ atomically $ initiateShutdownSTM val latch

-- | Signal shutdown with a specific result
initiateShutdownWithResult :: (MonadIO m) => Either e a -> ShutdownLatch e a -> m ()
initiateShutdownWithResult result latch = liftIO $ atomically $ initiateShutdownWithResultSTM result latch

-- | Get the shutdown result if available
getResult :: (MonadIO m) => ShutdownLatch e a -> m (Maybe (Either e a))
getResult latch = liftIO $ atomically $ getResultSTM latch

-- | Check if shutdown has been requested
isShuttingDown :: (MonadIO m) => ShutdownLatch e a -> m Bool
isShuttingDown latch = liftIO $ atomically $ isShuttingDownSTM latch

-- STM Operations for use in atomic transactions

-- | Check if shutdown is requested
isShuttingDownSTM :: ShutdownLatch e a -> STM Bool
isShuttingDownSTM latch = isJust <$> readTVar (unShutdownLatch latch)

-- | Signal shutdown with a "success" result
initiateShutdownSTM :: a -> ShutdownLatch e a -> STM ()
initiateShutdownSTM val latch = writeTVar (unShutdownLatch latch) (Just (Right val))

-- | Signal shutdown with a specific result
initiateShutdownWithResultSTM :: Either e a -> ShutdownLatch e a -> STM ()
initiateShutdownWithResultSTM result latch = writeTVar (unShutdownLatch latch) (Just result)

-- | Get the shutdown result (if available)
getResultSTM :: ShutdownLatch e a -> STM (Maybe (Either e a))
getResultSTM = readTVar . unShutdownLatch

-- | Block until shutdown is requested, then return the result
waitForShutdownSTM :: ShutdownLatch e a -> STM (Either e a)
waitForShutdownSTM latch = do
  mresult <- readTVar (unShutdownLatch latch)
  maybe retry return mresult
