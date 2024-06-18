module Cachix.Client.Daemon.PushManager.PushJob
  ( module Cachix.Client.Daemon.PushManager.PushJob,
    module Types,
  )
where

import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Types.PushManager as Types
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import Protolude

new :: (MonadIO m) => Protocol.PushRequest -> m PushJob
new pushRequest = do
  pushId <- Protocol.newPushRequestId
  timestamp <- liftIO getCurrentTime
  return $
    PushJob
      { pushId,
        pushRequest,
        pushStatus = Queued,
        pushQueue = mempty,
        pushResult = mempty,
        pushStats = newStats timestamp
      }

newStats :: UTCTime -> JobStats
newStats createdAt =
  JobStats
    { jsCreatedAt = createdAt,
      jsStartedAt = Nothing,
      jsCompletedAt = Nothing
    }

data ResolvedClosure p = ResolvedClosure
  { rcAllPaths :: Set p,
    rcMissingPaths :: Set p
  }

populateQueue :: ResolvedClosure FilePath -> UTCTime -> PushJob -> PushJob
populateQueue ResolvedClosure {..} timestamp pushJob@PushJob {..} = do
  let skippedPaths = Set.difference rcAllPaths rcMissingPaths
  pushJob
    { pushStatus = Running,
      pushStats = pushStats {jsStartedAt = Just timestamp},
      pushQueue = rcMissingPaths,
      pushResult = pushResult {prSkippedPaths = skippedPaths}
    }

addPushedPath :: FilePath -> PushResult -> PushResult
addPushedPath storePath pushResult =
  pushResult {prPushedPaths = Set.insert storePath (prPushedPaths pushResult)}

addFailedPath :: FilePath -> PushResult -> PushResult
addFailedPath storePath pushResult =
  pushResult {prFailedPaths = Set.insert storePath (prFailedPaths pushResult)}

markStorePathPushed :: FilePath -> PushJob -> PushJob
markStorePathPushed storePath pushJob@(PushJob {pushQueue, pushResult}) =
  pushJob
    { pushQueue = Set.delete storePath pushQueue,
      pushResult = addPushedPath storePath pushResult
    }

markStorePathFailed :: FilePath -> PushJob -> PushJob
markStorePathFailed storePath pushJob@(PushJob {pushQueue, pushResult}) =
  pushJob
    { pushQueue = Set.delete storePath pushQueue,
      pushResult = addFailedPath storePath pushResult
    }

status :: PushJob -> JobStatus
status PushJob {pushStatus} = pushStatus

queue :: PushJob -> Set FilePath
queue PushJob {pushQueue} = pushQueue

result :: PushJob -> PushResult
result PushJob {pushResult} = pushResult

hasQueuedPaths :: PushJob -> Bool
hasQueuedPaths = not . Set.null . queue

hasFailedPaths :: PushJob -> Bool
hasFailedPaths = not . Set.null . prFailedPaths . result

complete :: UTCTime -> PushJob -> PushJob
complete timestamp pushJob@PushJob {..} = do
  pushJob
    { pushStatus =
        case pushStatus of
          Running -> Completed
          _ -> pushStatus,
      pushStats = pushStats {jsCompletedAt = Just timestamp}
    }

fail :: UTCTime -> PushJob -> PushJob
fail timestamp pushJob@PushJob {..} = do
  pushJob
    { pushStatus = Failed,
      pushStats = pushStats {jsCompletedAt = Just timestamp}
    }

isCompleted :: PushJob -> Bool
isCompleted PushJob {pushStatus} = pushStatus == Completed

isFailed :: PushJob -> Bool
isFailed PushJob {pushStatus} = pushStatus == Failed

isProcessed :: PushJob -> Bool
isProcessed pushJob = isCompleted pushJob || isFailed pushJob

startedAt :: PushJob -> Maybe UTCTime
startedAt PushJob {pushStats = JobStats {jsStartedAt}} = jsStartedAt

completedAt :: PushJob -> Maybe UTCTime
completedAt PushJob {pushStats = JobStats {jsCompletedAt}} = jsCompletedAt

duration :: PushJob -> Maybe NominalDiffTime
duration PushJob {pushStats} = do
  t1 <- jsStartedAt pushStats
  t2 <- jsCompletedAt pushStats
  pure $ diffUTCTime t2 t1
