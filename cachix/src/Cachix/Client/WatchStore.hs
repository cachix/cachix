module Cachix.Client.WatchStore
  ( startWorkers,
  )
where

import Cachix.Client.CNix (filterInvalidStorePath)
import Cachix.Client.Push
import qualified Cachix.Client.PushQueue as PushQueue
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import Hercules.CNix.Store (Store)
import qualified Hercules.CNix.Store as Store
import Protolude
import System.FSNotify
import qualified System.Systemd.Daemon as Systemd

startWorkers :: Store -> Int -> PushParams IO () -> IO ()
startWorkers store numWorkers pushParams = do
  void Systemd.notifyReady
  withManager $ \mgr -> PushQueue.startWorkers numWorkers (producer store mgr) pushParams

producer :: Store -> WatchManager -> PushQueue.Queue -> IO (IO ())
producer store mgr queue = do
  putErrText "Watching /nix/store for new store paths ..."
  watchDir mgr "/nix/store" filterOnlyStorePaths (queueStorePathAction store queue)

queueStorePathAction :: Store -> PushQueue.Queue -> Event -> IO ()
queueStorePathAction store queue (Removed lockFile _ _) = do
  sp <- Store.parseStorePath store (encodeUtf8 $ toS $ dropLast 5 lockFile)
  filterInvalidStorePath store sp >>= \case
    Nothing -> return ()
    Just p -> atomically $ TBQueue.writeTBQueue queue p
queueStorePathAction _ _ _ = return ()

dropLast :: Int -> [a] -> [a]
dropLast index xs = take (length xs - index) xs

-- we queue store paths after their lock has been removed
filterOnlyStorePaths :: ActionPredicate
filterOnlyStorePaths (Removed fp _ _)
  | ".drv.lock" `isSuffixOf` fp = False
  | ".lock" `isSuffixOf` fp = True
filterOnlyStorePaths _ = False
