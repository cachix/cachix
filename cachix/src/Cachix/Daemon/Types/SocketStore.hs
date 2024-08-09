module Cachix.Daemon.Types.SocketStore (Socket (..), SocketId, SocketStore (..)) where

import Control.Concurrent.STM.TVar (TVar)
import Data.HashMap.Strict (HashMap)
import Data.UUID (UUID)
import Network.Socket qualified as Network (Socket)
import Protolude

data Socket = Socket
  { socketId :: SocketId,
    socket :: Network.Socket,
    handlerThread :: Async ()
  }

instance Eq Socket where
  (==) = (==) `on` socketId

instance Ord Socket where
  compare = comparing socketId

type SocketId = UUID

newtype SocketStore = SocketStore (TVar (HashMap SocketId Socket))
