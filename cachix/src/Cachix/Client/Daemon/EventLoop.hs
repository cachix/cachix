module Cachix.Client.Daemon.EventLoop
  ( new,
    send,
    sendIO,
    run,
    exitLoopWith,
    EventLoop,
    DaemonEvent (..),
  )
where

import Cachix.Client.Daemon.Types.EventLoop (DaemonEvent (..), EventLoop (..))
import Control.Concurrent.STM.TBMQueue
  ( isFullTBMQueue,
    newTBMQueueIO,
    readTBMQueue,
    tryWriteTBMQueue,
  )
import Data.Text.Lazy.Builder (toLazyText)
import qualified Katip
import Protolude

new :: (MonadIO m) => m EventLoop
new = do
  exitLatch <- liftIO newEmptyMVar
  queue <- liftIO $ newTBMQueueIO 100
  return $ EventLoop {queue, exitLatch}

-- | Send an event to the event loop with logging.
send :: (Katip.KatipContext m) => EventLoop -> DaemonEvent -> m ()
send = send' Katip.logFM

-- | Same as 'send', but does not require a 'Katip.KatipContext'.
sendIO :: forall m. (MonadIO m) => EventLoop -> DaemonEvent -> m ()
sendIO = send' logger
  where
    logger :: Katip.Severity -> Katip.LogStr -> m ()
    logger Katip.ErrorS msg = liftIO $ hPutStrLn stderr (toLazyText $ Katip.unLogStr msg)
    logger _ _ = return ()

send' :: (MonadIO m) => (Katip.Severity -> Katip.LogStr -> m ()) -> EventLoop -> DaemonEvent -> m ()
send' logger eventloop@(EventLoop {queue}) event = do
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
      exitLoopWith (ExitFailure 1) eventloop

run :: (MonadIO m) => EventLoop -> (DaemonEvent -> m ()) -> m ExitCode
run eventloop f = do
  fix $ \loop -> do
    mevent <- liftIO $ atomically $ readTBMQueue (queue eventloop)
    case mevent of
      Just event -> f event
      Nothing -> exitLoopWith (ExitFailure 1) eventloop

    liftIO (tryReadMVar (exitLatch eventloop)) >>= \case
      Just exitCode -> return exitCode
      Nothing -> loop

exitLoopWith :: (MonadIO m) => ExitCode -> EventLoop -> m ()
exitLoopWith exitCode EventLoop {exitLatch} = void $ liftIO $ tryPutMVar exitLatch exitCode
