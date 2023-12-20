{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cachix.Client.Daemon.Types.PushManager
  ( PushManagerEnv (..),
    PushManager (..),
    PushJob (..),
    PushDetails (..),
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

data PushManagerEnv = PushManagerEnv
  { -- | Active push jobs.
    pmPushJobs :: TVar (HashMap Protocol.PushRequestId PushJob),
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

-- | A push request that has been queued for processing.
data PushJob = PushJob
  { -- | A unique identifier for this push request.
    pushId :: Protocol.PushRequestId,
    -- | The time when the push request was queued.
    pushCreatedAt :: UTCTime,
    -- | The time when the push request was started.
    pushStartedAt :: Maybe UTCTime,
    -- | The time when the push request was finished.
    pushFinishedAt :: Maybe UTCTime,
    -- | The original push request.
    pushRequest :: Protocol.PushRequest,
    -- | The state of the push request.
    pushDetails :: PushDetails
  }
  deriving stock (Eq, Show)

data PushDetails = PushDetails
  { pdAllPaths :: Set FilePath,
    pdQueuedPaths :: Set FilePath,
    pdPushedPaths :: Set FilePath,
    pdSkippedPaths :: Set FilePath,
    pdFailedPaths :: Set FilePath
  }
  deriving stock (Eq, Ord, Show)
