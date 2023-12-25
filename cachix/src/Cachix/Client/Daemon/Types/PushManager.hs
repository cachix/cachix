{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cachix.Client.Daemon.Types.PushManager
  ( PushManagerEnv (..),
    PushManager (..),
    PushJob (..),
    JobStatus (..),
    JobStats (..),
    PushResult (..),
    OnPushEvent,
    Task (..),
  )
where

import qualified Cachix.Client.Daemon.Log as Log
import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Types.Log (Logger)
import Cachix.Client.Daemon.Types.PushEvent (PushEvent (..))
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TVar
import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.HashMap.Strict (HashMap)
import Data.Time (UTCTime)
import qualified Katip
import Protolude

data Task
  = ResolveClosure Protocol.PushRequestId
  | PushStorePath FilePath

type PushJobStore = TVar (HashMap Protocol.PushRequestId PushJob)

data PushManagerEnv = PushManagerEnv
  { pmPushJobs :: PushJobStore,
    -- | A mapping of store paths to to push requests.
    -- Use to prevent duplicate pushes and track with store paths are referenced by push requests.
    pmStorePathReferences :: TVar (HashMap FilePath [Protocol.PushRequestId]),
    -- | FIFO queue of push tasks.
    pmTaskQueue :: TBMQueue Task,
    pmTaskSemaphore :: QSem,
    -- | Callback for push events.
    pmOnPushEvent :: OnPushEvent,
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
      MonadThrow
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
