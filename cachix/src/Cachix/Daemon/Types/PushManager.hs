{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cachix.Daemon.Types.PushManager
  ( PushManagerEnv (..),
    PushManager (..),
    PushJobStore,
    PushJob (..),
    JobStatus (..),
    JobStats (..),
    PushResult (..),
    OnPushEvent,
    Task (..),
    TimeoutOptions (..),
  )
where

import Cachix.Client.Push (PushParams)
import Cachix.Daemon.Log qualified as Log
import Cachix.Daemon.NarinfoBatch (NarinfoBatchManager)
import Cachix.Daemon.NarinfoBatch qualified as NarinfoBatch
import Cachix.Daemon.Protocol qualified as Protocol
import Cachix.Daemon.Types.Log (Logger)
import Cachix.Daemon.Types.PushEvent (PushEvent (..))
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TVar
import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.HashMap.Strict (HashMap)
import Data.Time (UTCTime)
import Katip qualified
import Protolude

data Task
  = ResolveClosure Protocol.PushRequestId
  | ProcessBatchResponse Protocol.PushRequestId NarinfoBatch.BatchResponse
  | PushStorePath FilePath

type PushJobStore = TVar (HashMap Protocol.PushRequestId PushJob)

type StorePathIndex = TVar (HashMap FilePath [Protocol.PushRequestId])

-- TODO: a lot of the logic surrounding deduping, search, and job tracking could be replaced by sqlite.
-- sqlite can run in-memory if we don't need persistence.
-- If we do, then we can we get stop/resume for free.
data PushManagerEnv = PushManagerEnv
  { pmPushParams :: PushParams PushManager (),
    -- | A store of push jobs indexed by a PushRequestId.
    pmPushJobs :: PushJobStore,
    -- | A mapping of store paths to push requests.
    -- Used when deduplicating pushes.
    pmStorePathIndex :: StorePathIndex,
    -- | FIFO queue of push tasks.
    pmTaskQueue :: TBMQueue Task,
    -- | A semaphore to control task concurrency.
    pmTaskSemaphore :: QSem,
    -- | A callback for push events.
    pmOnPushEvent :: OnPushEvent,
    -- | The timestamp of the most recent event. This is used to track activity internally.
    pmLastEventTimestamp :: TVar UTCTime,
    -- | The number of pending (uncompleted) jobs.
    pmPendingJobCount :: TVar Int,
    -- | Manager for batching narinfo queries
    pmNarinfoBatchManager :: NarinfoBatchManager Protocol.PushRequestId,
    pmLogger :: Logger
  }

type OnPushEvent = Protocol.PushRequestId -> PushEvent -> IO ()

newtype PushManager a = PushManager {unPushManager :: ReaderT PushManagerEnv IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadUnliftIO,
      MonadReader PushManagerEnv,
      MonadCatch,
      MonadMask,
      MonadThrow,
      Alternative,
      MonadPlus
    )

instance Katip.Katip PushManager where
  getLogEnv = Log.getKatipLogEnv <$> asks pmLogger
  localLogEnv f (PushManager m) = PushManager (local (\s -> s {pmLogger = Log.localLogEnv f (pmLogger s)}) m)

instance Katip.KatipContext PushManager where
  getKatipContext = Log.getKatipContext <$> asks pmLogger
  localKatipContext f (PushManager m) = PushManager (local (\s -> s {pmLogger = Log.localKatipContext f (pmLogger s)}) m)

  getKatipNamespace = Log.getKatipNamespace <$> asks pmLogger
  localKatipNamespace f (PushManager m) = PushManager (local (\s -> s {pmLogger = Log.localKatipNamespace f (pmLogger s)}) m)

data JobStatus = Queued | Running | Completed | Failed
  deriving stock (Eq, Show)

-- | A push request that has been queued for processing.
data PushJob = PushJob
  { -- | A unique identifier for this push request.
    pushId :: Protocol.PushRequestId,
    -- | The original push request.
    pushRequest :: Protocol.PushRequest,
    -- | The current status of the push job.
    pushStatus :: JobStatus,
    -- | Paths that need to be pushed.
    pushQueue :: Set FilePath,
    -- | Track whether paths were pushed, skipped, or failed to push.
    pushResult :: PushResult,
    -- | Timing stats for the push job.
    pushStats :: JobStats
  }
  deriving stock (Eq, Show)

data PushResult = PushResult
  { prPushedPaths :: Set FilePath,
    prFailedPaths :: Set FilePath,
    prSkippedPaths :: Set FilePath
  }
  deriving stock (Eq, Show)

instance Semigroup PushResult where
  PushResult a1 b1 c1 <> PushResult a2 b2 c2 =
    PushResult (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid PushResult where
  mempty = PushResult mempty mempty mempty

data JobStats = JobStats
  { jsCreatedAt :: UTCTime,
    jsStartedAt :: Maybe UTCTime,
    jsCompletedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show)

data TimeoutOptions = TimeoutOptions
  { -- | The maximum time to wait in seconds.
    toTimeout :: Float,
    -- | The interval at which to check the timeout condition.
    toPollingInterval :: Float
  }
  deriving stock (Eq, Show)
