{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Cachix.Client.Daemon.Types.PushManager
  ( PushManagerEnv (..),
    PushManager (..),
    PushJob (..),
    PushDetails (..),
  )
where

import qualified Cachix.Client.Daemon.Log as Log
import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Subscription (SubscriptionManager)
import qualified Cachix.Client.Daemon.Subscription as Subscription
import Cachix.Client.Daemon.Types.Log (Logger)
import Cachix.Client.Daemon.Types.PushEvent (PushEvent (..))
import Control.Concurrent.STM.TVar
import Control.Monad.Catch
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Retry (RetryStatus (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime, getCurrentTime)
import qualified Katip
import Protolude

data PushManagerEnv = PushManagerEnv
  { pmPushRequests :: TVar (Map Protocol.PushRequestId PushJob),
    pmStorePaths :: TVar (Map Text (Set Protocol.PushRequestId)),
    pmLogger :: Logger
  }

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
    -- | The original push request.
    pushRequest :: Protocol.PushRequest,
    -- | The state of the push request.
    pushDetails :: PushDetails
  }
  deriving (Eq)

data PushDetails = PushDetails
  { pdAllPaths :: Set Text,
    pdQueuedPaths :: Set Text,
    pdPushedPaths :: Set Text,
    pdSkippedPaths :: Set Text,
    pdFailedPaths :: Set Text
  }
  deriving stock (Eq, Ord)
