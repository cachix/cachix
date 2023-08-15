module Cachix.Client.Daemon.Types where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.URI
import qualified Data.Aeson as Aeson
import qualified Network.Socket as Socket
import Protolude
import Servant.Auth.Client (Token)

data ClientMessage
  = ClientPushRequest PushRequest
  | ClientStop
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | A request sent to the daemon to push a store path to a binary cache.
data PushRequest = PushRequest
  { authToken :: Token,
    binaryCacheName :: Text,
    -- Host is not currently supported
    host :: URI,
    storePaths :: [FilePath]
  }
  deriving stock (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data QueuedPushRequest = QueuedPushRequest
  { pushRequest :: PushRequest,
    -- | An open socket to the client that sent the push request.
    clientConnection :: Socket.Socket
  }
