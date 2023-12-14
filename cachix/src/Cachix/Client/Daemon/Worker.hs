module Cachix.Client.Daemon.Worker
  ( startWorkers,
    stopWorkers,
    startWorker,
    stopWorker,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Types (Daemon)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM.TBMQueue
import qualified Control.Immortal as Immortal
import qualified Katip
import Protolude

startWorkers :: Int -> TBMQueue a -> (a -> Daemon ()) -> Daemon [Immortal.Thread]
startWorkers numWorkers queue f = do
  replicateM numWorkers (startWorker queue f)

startWorker :: TBMQueue a -> (a -> Daemon ()) -> Daemon Immortal.Thread
startWorker queue f = do
  Immortal.createWithLabel "worker" $ \thread -> do
    Katip.katipAddNamespace "worker" $
      Immortal.onUnexpectedFinish thread logWorkerException (runWorker queue f)

stopWorkers :: [Immortal.Thread] -> Daemon ()
stopWorkers workers = do
  Katip.logFM Katip.DebugS "Waiting for workers to finish..."
  liftIO $ Async.mapConcurrently_ stopWorker workers
  Katip.logFM Katip.DebugS "Workers finished."

stopWorker :: Immortal.Thread -> IO ()
stopWorker worker = do
  Immortal.mortalize worker
  Immortal.wait worker

logWorkerException :: (Exception e) => Either e () -> Daemon ()
logWorkerException (Left err) =
  Katip.logFM Katip.ErrorS $ Katip.ls (toS $ displayException err :: Text)
logWorkerException _ = return ()

runWorker :: TBMQueue a -> (a -> Daemon ()) -> Daemon ()
runWorker queue f = loop
  where
    loop = do
      mres <- liftIO $ atomically $ readTBMQueue queue

      case mres of
        Nothing -> return ()
        Just job -> do
          f job
          loop
