module Cachix.Client.WatchStore
  ( startWorkers,
  )
where

import Cachix.Client.CNix (logStorePathWarning, resolveStorePath)
import Cachix.Client.Push
import Cachix.Client.PushQueue qualified as PushQueue
import Control.Concurrent.STM.TBQueue qualified as TBQueue
import Hercules.CNix.Store (Store)
import Hercules.CNix.Store qualified as Store
import Protolude hiding (toS)
import Protolude.Conv
import System.FSNotify
import System.Systemd.Daemon qualified as Systemd

startWorkers :: Store -> Int -> PushParams IO () -> IO ()
startWorkers store numWorkers pushParams = do
  storeDirectory <- toS <$> Store.storeDir store
  void Systemd.notifyReady
  withManager $ \mgr -> PushQueue.startWorkers numWorkers (producer storeDirectory store mgr) pushParams

producer :: FilePath -> Store -> WatchManager -> PushQueue.Queue -> IO (IO ())
producer storeDirectory store mgr queue = do
  putErrText $ "Watching " <> toS storeDirectory <> " for new store paths ..."
  watchDir mgr storeDirectory filterOnlyStorePaths (queueStorePathAction store queue)

queueStorePathAction :: Store -> PushQueue.Queue -> Event -> IO ()
queueStorePathAction store queue (Removed lockFile _ _) = do
  let filePath = dropLast 5 lockFile
  resolveStorePath store filePath >>= \case
    Left err -> logStorePathWarning filePath err
    Right p -> atomically $ TBQueue.writeTBQueue queue p
queueStorePathAction _ _ _ = return ()

dropLast :: Int -> [a] -> [a]
dropLast index xs = take (length xs - index) xs

-- we queue store paths after their lock has been removed
filterOnlyStorePaths :: ActionPredicate
filterOnlyStorePaths (Removed fp _ _)
  | ".drv.lock" `isSuffixOf` fp = False
  | ".lock" `isSuffixOf` fp = True
filterOnlyStorePaths _ = False
