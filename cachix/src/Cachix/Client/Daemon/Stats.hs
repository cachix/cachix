module Cachix.Client.Daemon.Stats where

import Cachix.Client.Daemon.Types (Daemon, DaemonEnv (..), PushStatistics (..))
import Control.Concurrent.STM.TVar
import Protolude
import System.Console.Pretty

modifyPushStats :: (PushStatistics -> PushStatistics) -> Daemon ()
modifyPushStats f = liftIO . atomically . flip modifyTVar' f =<< asks daemonStats

incrementPushedStat :: Daemon ()
incrementPushedStat =
  modifyPushStats $ \stats ->
    stats
      { pushedCount = pushedCount stats + 1,
        totalCount = totalCount stats + 1
      }

incrementFailedStat :: Daemon ()
incrementFailedStat =
  modifyPushStats $ \stats ->
    stats
      { failedCount = failedCount stats + 1,
        totalCount = totalCount stats + 1
      }

incrementSkippedStat :: Daemon ()
incrementSkippedStat = modifyPushStats $ \stats ->
  stats
    { skippedCount = skippedCount stats + 1,
      totalCount = totalCount stats + 1
    }

renderStats :: DaemonEnv -> IO Text
renderStats DaemonEnv {daemonStats} = do
  PushStatistics {..} <- liftIO $ readTVarIO daemonStats
  supportsColor <- supportsPretty
  let myColor c = if supportsColor then color c else identity
  pure $
    mconcat
      [ "[",
        myColor Green $ "pushed " <> show pushedCount,
        "][skipped ",
        show skippedCount,
        "][",
        (if failedCount > 0 then myColor Red else identity) $ "failed " <> show failedCount,
        "][",
        "total " <> show totalCount,
        "]"
      ]
