module Cachix.Client.Daemon.Worker
  ( startWorkers,
    stopWorkers,
    startWorker,
    stopWorker,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.ShutdownLatch
import Cachix.Client.Daemon.Types (Daemon, DaemonEnv (..), QueuedPushRequest)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM.TBMQueue
import qualified Control.Immortal as Immortal
import qualified Katip
import Protolude

startWorkers :: Int -> (QueuedPushRequest -> Daemon ()) -> Daemon [Immortal.Thread]
startWorkers numWorkers f = do
  replicateM numWorkers (startWorker f)

stopWorkers :: [Immortal.Thread] -> Daemon ()
stopWorkers workers = do
  Katip.logFM Katip.InfoS "Waiting for workers to finish..."
  liftIO $ Async.mapConcurrently_ stopWorker workers
  Katip.logFM Katip.InfoS "Workers finished."

startWorker :: (QueuedPushRequest -> Daemon ()) -> Daemon Immortal.Thread
startWorker f = do
  Immortal.createWithLabel "worker" $ \thread -> do
    Katip.katipAddNamespace "worker" $
      Immortal.onUnexpectedFinish thread logWorkerException (runWorker f)

stopWorker :: Immortal.Thread -> IO ()
stopWorker worker = do
  Immortal.mortalize worker
  Immortal.wait worker

logWorkerException :: (Exception e) => Either e () -> Daemon ()
logWorkerException (Left err) =
  Katip.logFM Katip.ErrorS $ Katip.ls (toS $ displayException err :: Text)
logWorkerException _ = return ()

runWorker :: (QueuedPushRequest -> Daemon ()) -> Daemon ()
runWorker f = loop
  where
    loop = do
      DaemonEnv {..} <- ask
      mres <- liftIO $ atomically (readTBMQueue daemonQueue)
      case mres of
        Nothing -> return ()
        Just msg -> do
          f msg
          loop
