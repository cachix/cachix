module Cachix.Client.Daemon.Types.PushEvent
  ( PushEvent (..),
    PushEventMessage (..),
    PushRetryStatus (..),
    newPushRetryStatus,
  )
where

import qualified Cachix.Client.Daemon.Protocol as Protocol
import Control.Retry (RetryStatus (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime)
import Protolude

data PushEvent = PushEvent
  { -- TODO: newtype a monotonic clock
    eventTimestamp :: UTCTime,
    eventPushId :: Protocol.PushRequestId,
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
