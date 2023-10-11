module Cachix.Client.Daemon.Types where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.URI
import qualified Data.Aeson as Aeson
import qualified Network.Socket as Socket
import Protolude
import Servant.Auth.Client (Token)

-- | JSON messages that the client can send to the daemon
data ClientMessage
  = ClientPushRequest PushRequest
  | ClientStop
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data DaemonMessage
  = DaemonBye
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | A request for the daemon to push store paths to a binary cache
data PushRequest = PushRequest
  { authToken :: Token,
    binaryCacheName :: Text,
    host :: URI, -- TODO: host is not currently supported
    storePaths :: [FilePath]
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data QueuedPushRequest = QueuedPushRequest
  { -- | The original push request
    pushRequest :: PushRequest,
    -- | An open socket to the client that sent the push request.
    clientConnection :: Socket.Socket
  }
