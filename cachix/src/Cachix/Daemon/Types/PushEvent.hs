module Cachix.Daemon.Types.PushEvent
  ( PushEvent (..),
    PushEventMessage (..),
    PushRetryStatus (..),
    newPushRetryStatus,
    PushRequestId,
    newPushRequestId,
  )
where

import Control.Retry (RetryStatus (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Protolude
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Aeson qualified as Aeson

data PushEvent = PushEvent
  { -- TODO: newtype a monotonic clock
    eventTimestamp :: UTCTime,
    eventPushId :: PushRequestId,
    eventMessage :: PushEventMessage
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Ord PushEvent where
  compare = compare `on` eventTimestamp

data PushEventMessage
  = PushStarted
  | PushStorePathAttempt FilePath Int64 PushRetryStatus
  | PushStorePathProgress FilePath Int64 Int64
  | PushStorePathDone FilePath
  | PushStorePathFailed FilePath Text
  | PushFinished
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data PushRetryStatus = PushRetryStatus {retryCount :: Int}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newPushRetryStatus :: RetryStatus -> PushRetryStatus
newPushRetryStatus RetryStatus {..} = PushRetryStatus {retryCount = rsIterNumber}

newtype PushRequestId = PushRequestId UUID
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Aeson.FromJSON, Aeson.ToJSON, Hashable)

newPushRequestId :: (MonadIO m) => m PushRequestId
newPushRequestId = liftIO $ PushRequestId <$> UUID.nextRandom
