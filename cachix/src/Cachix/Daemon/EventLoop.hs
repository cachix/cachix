module Cachix.Daemon.EventLoop
  ( new,
    send,
    sendIO,
    run,
    exitLoopWith,
    exitLoopWithFailure,
    EventLoop,
  )
where

import Cachix.Daemon.ShutdownLatch qualified as ShutdownLatch
import Cachix.Daemon.Types.EventLoop (EventLoop (..), EventLoopError (..))
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMQueue
  ( isFullTBMQueue,
    newTBMQueueIO,
    readTBMQueue,
    tryWriteTBMQueue,
  )
import Data.Text.Lazy.Builder (toLazyText)
import Katip qualified
import Protolude
import Cachix.Daemon.Types.Daemon (DaemonEvent)

new :: (MonadIO m) => m (EventLoop event a)
new = do
  shutdownLatch <- ShutdownLatch.newShutdownLatch
  queue <- liftIO $ newTBMQueueIO 100_000
  return $ EventLoop {queue, shutdownLatch}

-- | Send an event to the event loop with logging.
send :: (Katip.KatipContext m) => EventLoop DaemonEvent a -> DaemonEvent -> m ()
send = send' Katip.logFM

-- | Same as 'send', but does not require a 'Katip.KatipContext'.
sendIO :: (MonadIO m) => EventLoop DaemonEvent a -> DaemonEvent -> m ()
sendIO = send' logger
  where
    logger :: MonadIO m => Katip.Severity -> Katip.LogStr -> m ()
    logger Katip.ErrorS msg = liftIO $ hPutStrLn stderr (toLazyText $ Katip.unLogStr msg)
    logger _ _ = return ()

send' :: (MonadIO m) => (Katip.Severity -> Katip.LogStr -> m ()) -> EventLoop DaemonEvent a -> DaemonEvent -> m ()
send' logger eventloop@(EventLoop {queue, shutdownLatch}) event = do
  -- First check if shutdown has been requested
  isExiting <- ShutdownLatch.isShuttingDown shutdownLatch
  if isExiting
    then logger Katip.DebugS "Ignored an event because the event loop is shutting down"
    else do
      res <- liftIO $ atomically $ tryWriteTBMQueue queue event
      case res of
        -- The queue is closed.
        Nothing ->
          logger Katip.DebugS "Ignored an event because the event loop is closed"
        -- Successfully wrote to the queue
        Just True -> return ()
        -- Failed to write to the queue
        Just False -> do
          isFull <- liftIO $ atomically $ isFullTBMQueue queue
          let message =
                if isFull
                  then "Event loop is full"
                  else "Unknown error"
          logger Katip.ErrorS $ "Failed to write to event loop: " <> message
          exitLoopWithFailure EventLoopFull eventloop

-- | Run the event loop until it exits with 'exitLoopWith'.
run :: (MonadIO m) => EventLoop event a -> (event -> m ()) -> m (Either EventLoopError a)
run (EventLoop {queue, shutdownLatch}) f =
  fix $ \loop -> do
    -- Wait for either a shutdown signal or a message from the queue
    eitherResult <-
      liftIO $
        atomically $
          fmap Left (ShutdownLatch.waitForShutdownSTM shutdownLatch)
            `orElse`
            -- Try to read from queue
            ( do
                mevent <- readTBMQueue queue
                case mevent of
                  -- Got an event, return it
                  Just event -> return $ Right event
                  -- Queue is closed, signal shutdown
                  Nothing -> do
                    ShutdownLatch.initiateShutdownWithResultSTM (Left EventLoopClosed) shutdownLatch
                    result <- ShutdownLatch.waitForShutdownSTM shutdownLatch
                    return $ Left result
            )

    -- Process the result
    case eitherResult of
      -- Shutdown requested, return the result
      Left result -> return result
      -- Got an event, process it and continue looping
      Right event -> do
        f event
        loop

-- | Short-circuit the event loop and exit with a given return value.
exitLoopWith :: (MonadIO m) => a -> EventLoop event a -> m ()
exitLoopWith exitValue (EventLoop {shutdownLatch}) =
  ShutdownLatch.initiateShutdown exitValue shutdownLatch

-- | Short-circuit the event loop in case of an internal error.
exitLoopWithFailure :: (MonadIO m) => EventLoopError -> EventLoop event a -> m ()
exitLoopWithFailure err (EventLoop {shutdownLatch}) =
  ShutdownLatch.initiateShutdownWithResult (Left err) shutdownLatch
