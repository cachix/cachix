module Cachix.Client.Daemon.Worker
  ( startWorkers,
    stopWorkers,
    startWorker,
    stopWorker,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Types (Daemon, DaemonEnv (..), PushJob (..))
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TVar
import qualified Control.Immortal as Immortal
import qualified Data.Map.Strict as Map
import qualified Katip
import Protolude

startWorkers :: Int -> (PushJob -> Daemon ()) -> Daemon [Immortal.Thread]
startWorkers numWorkers f = do
  replicateM numWorkers (startWorker f)

startWorker :: (PushJob -> Daemon ()) -> Daemon Immortal.Thread
startWorker f = do
  Immortal.createWithLabel "worker" $ \thread -> do
    Katip.katipAddNamespace "worker" $
      Immortal.onUnexpectedFinish thread logWorkerException (runWorker f)

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

runWorker :: (PushJob -> Daemon ()) -> Daemon ()
runWorker f = loop
  where
    loop = do
      DaemonEnv {..} <- ask
      mres <- liftIO $ atomically $ readTBMQueue daemonQueue

      case mres of
        Nothing -> return ()
        Just job -> do
          f job
          loop
