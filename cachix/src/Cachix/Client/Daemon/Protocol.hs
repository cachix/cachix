{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cachix.Client.Daemon.Protocol
  ( ClientMessage (..),
    DaemonMessage (..),
    PushRequestId,
    newPushRequestId,
    PushRequest (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Protolude

-- | JSON messages that the client can send to the daemon
data ClientMessage
  = ClientPushRequest PushRequest
  | ClientStop
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | JSON messages that the daemon can send to the client
data DaemonMessage
  = DaemonBye
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

newtype PushRequestId = PushRequestId UUID
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Aeson.FromJSON, Aeson.ToJSON, Hashable)

newPushRequestId :: (MonadIO m) => m PushRequestId
newPushRequestId = liftIO $ PushRequestId <$> UUID.nextRandom

-- | A request for the daemon to push store paths to a binary cache
data PushRequest = PushRequest
  { storePaths :: [FilePath]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)
