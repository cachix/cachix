module Cachix.Client.WatchStore
  ( startWorkers,
  )
where

import Cachix.Client.Push
import qualified Cachix.Client.PushQueue as PushQueue
import qualified Cachix.Client.Store as Store
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import Protolude
import System.FSNotify
import qualified System.Systemd.Daemon as Systemd

startWorkers :: Store.Store -> Int -> PushParams IO () -> IO ()
startWorkers store numWorkers pushParams = do
  void Systemd.notifyReady
  withManager $ \mgr -> PushQueue.startWorkers numWorkers (producer store mgr) pushParams

producer :: Store.Store -> WatchManager -> PushQueue.Queue -> IO (IO ())
producer _ mgr queue = do
  putTextError "Watching /nix/store for new store paths ..."
  watchDir mgr "/nix/store" filterOnlyStorePaths (queueStorePathAction queue)

queueStorePathAction :: PushQueue.Queue -> Event -> IO ()
queueStorePathAction queue (Removed lockFile _ _) = do
  atomically $ TBQueue.writeTBQueue queue $ Store.StorePath (toS $ dropLast 5 lockFile)
queueStorePathAction _ _ = return ()

dropLast :: Int -> [a] -> [a]
dropLast index xs = take (length xs - index) xs

-- we queue store paths after their lock has been removed
filterOnlyStorePaths :: ActionPredicate
filterOnlyStorePaths (Removed fp _ _)
  | ".drv.lock" `isSuffixOf` fp = False
  | ".lock" `isSuffixOf` fp = True
filterOnlyStorePaths _ = False

putTextError :: Text -> IO ()
putTextError = hPutStrLn stderr
