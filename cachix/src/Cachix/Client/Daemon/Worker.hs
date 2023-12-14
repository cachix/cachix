module Cachix.Client.Daemon.Worker
  ( startWorkers,
    stopWorkers,
    startWorker,
    stopWorker,
  )
where

import Cachix.Client.Config.Orphans ()
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM.TBMQueue
import qualified Control.Immortal as Immortal
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Katip
import Protolude

startWorkers ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  Int ->
  TBMQueue a ->
  (a -> m ()) ->
  m [Immortal.Thread]
startWorkers numWorkers queue f = do
  replicateM numWorkers (startWorker queue f)

startWorker ::
  (MonadUnliftIO m, Katip.KatipContext m) =>
  TBMQueue a ->
  (a -> m ()) ->
  m Immortal.Thread
startWorker queue f = do
  Immortal.createWithLabel "worker" $ \thread -> do
    Katip.katipAddNamespace "worker" $
      Immortal.onUnexpectedFinish thread logWorkerException (runWorker queue f)

stopWorkers :: (Katip.KatipContext m) => [Immortal.Thread] -> m ()
stopWorkers workers = do
  Katip.logFM Katip.DebugS "Waiting for workers to finish..."
  liftIO $ Async.mapConcurrently_ stopWorker workers
  Katip.logFM Katip.DebugS "Workers finished."

stopWorker :: (MonadIO m) => Immortal.Thread -> m ()
stopWorker worker = liftIO $ do
  Immortal.mortalize worker
  Immortal.wait worker

logWorkerException :: (Exception e, Katip.KatipContext m) => Either e () -> m ()
logWorkerException (Left err) =
  Katip.logFM Katip.ErrorS $ Katip.ls (toS $ displayException err :: Text)
logWorkerException _ = return ()

runWorker :: (MonadIO m) => TBMQueue a -> (a -> m ()) -> m ()
runWorker queue f = loop
  where
    loop = do
      mres <- liftIO $ atomically $ readTBMQueue queue

      case mres of
        Nothing -> return ()
        Just job -> do
          f job
          loop
