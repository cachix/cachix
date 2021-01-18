module Cachix.Client.WatchStore
  ( startWorkers,
  )
where

import Cachix.Client.Push
import qualified Cachix.Client.PushQueue as PushQueue
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import Data.List (isSuffixOf)
import Protolude
import System.FSNotify

startWorkers :: Int -> PushParams IO () -> IO ()
startWorkers numWorkers pushParams = do
  withManager $ \mgr -> PushQueue.startWorkers numWorkers (producer mgr) pushParams

producer :: WatchManager -> PushQueue.Queue -> IO (IO ())
producer mgr queue = do
  putText "Watching /nix/store for new store paths ..."
  watchDir mgr "/nix/store" filterOnlyStorePaths (queueStorePathAction queue)

queueStorePathAction :: PushQueue.Queue -> Event -> IO ()
queueStorePathAction queue (Removed lockFile _ _) = atomically $ TBQueue.writeTBQueue queue (toS $ dropLast 5 lockFile)
queueStorePathAction _ _ = return ()

dropLast :: Int -> [a] -> [a]
dropLast index xs = take (length xs - index) xs

-- we queue store paths after their lock has been removed
filterOnlyStorePaths :: ActionPredicate
filterOnlyStorePaths (Removed fp _ _)
  | ".drv.lock" `isSuffixOf` fp = False
  | ".lock" `isSuffixOf` fp = True
filterOnlyStorePaths _ = False
