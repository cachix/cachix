module Cachix.Daemon.EventLoop
  ( new,
    send,
    sendIO,
    run,
    exitLoopWith,
    EventLoop,
  )
where

import Cachix.Daemon.Types.EventLoop (EventLoop (..), EventLoopError (..))
import Control.Concurrent.STM.TBMQueue
  ( isFullTBMQueue,
    newTBMQueueIO,
    readTBMQueue,
    tryWriteTBMQueue,
  )
import Data.Text.Lazy.Builder (toLazyText)
import Katip qualified
import Protolude

new :: (MonadIO m) => m (EventLoop event a)
new = do
  exitLatch <- liftIO newEmptyMVar
  queue <- liftIO $ newTBMQueueIO 100
  return $ EventLoop {queue, exitLatch}

-- | Send an event to the event loop with logging.
send :: (Katip.KatipContext m) => EventLoop event a -> event -> m ()
send = send' Katip.logFM

-- | Same as 'send', but does not require a 'Katip.KatipContext'.
sendIO :: forall m event a. (MonadIO m) => EventLoop event a -> event -> m ()
sendIO = send' logger
  where
    logger :: Katip.Severity -> Katip.LogStr -> m ()
    logger Katip.ErrorS msg = liftIO $ hPutStrLn stderr (toLazyText $ Katip.unLogStr msg)
    logger _ _ = return ()

send' :: (MonadIO m) => (Katip.Severity -> Katip.LogStr -> m ()) -> EventLoop event a -> event -> m ()
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
      exitLoopWithFailure EventLoopFull eventloop

-- | Run the event loop until it exits with 'exitLoopWith'.
run :: (MonadIO m) => EventLoop event a -> (event -> m ()) -> m (Either EventLoopError a)
run eventloop f = do
  fix $ \loop -> do
    mevent <- liftIO $ atomically $ readTBMQueue (queue eventloop)
    case mevent of
      Just event -> f event
      Nothing -> exitLoopWithFailure EventLoopClosed eventloop

    liftIO (tryReadMVar (exitLatch eventloop)) >>= \case
      Just exitValue -> return exitValue
      Nothing -> loop

-- | Short-circuit the event loop and exit with a given return value.
exitLoopWith :: (MonadIO m) => a -> EventLoop event a -> m ()
exitLoopWith exitValue (EventLoop {exitLatch}) = void $ liftIO $ tryPutMVar exitLatch (Right exitValue)

-- | Short-circuit the event loop in case of an internal error.
exitLoopWithFailure :: (MonadIO m) => EventLoopError -> EventLoop event a -> m ()
exitLoopWithFailure err (EventLoop {exitLatch}) = void $ liftIO $ tryPutMVar exitLatch (Left err)
