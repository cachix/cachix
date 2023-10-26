module Cachix.Client.Daemon.Protocol where

import qualified Data.Aeson as Aeson
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

-- | A request for the daemon to push store paths to a binary cache
data PushRequest = PushRequest
  { storePaths :: [FilePath]
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)
