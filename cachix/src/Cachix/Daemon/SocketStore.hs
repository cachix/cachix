module Cachix.Daemon.SocketStore
  ( newSocketStore,
    addSocket,
    removeSocket,
    toList,
    Socket (..),
  )
where

import Cachix.Daemon.Types.SocketStore (Socket (..), SocketId, SocketStore (..))
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.HashMap.Strict qualified as HashMap
import Data.UUID.V4 qualified as UUID
import Network.Socket qualified
import Protolude hiding (toList)
import UnliftIO.Async qualified as Async

newSocketStore :: (MonadIO m) => m SocketStore
newSocketStore = SocketStore <$> liftIO (newTVarIO mempty)

newSocketId :: (MonadIO m) => m SocketId
newSocketId = liftIO UUID.nextRandom

addSocket :: (MonadUnliftIO m) => Network.Socket.Socket -> (SocketId -> Network.Socket.Socket -> m ()) -> SocketStore -> m ()
addSocket socket handler (SocketStore st) = do
  socketId <- newSocketId
  handlerThread <- Async.async (handler socketId socket)
  liftIO $ atomically $ modifyTVar' st $ HashMap.insert socketId (Socket {..})

removeSocket :: (MonadIO m) => SocketId -> SocketStore -> m ()
removeSocket socketId (SocketStore stvar) = do
  msocket <- liftIO $ atomically $ stateTVar stvar $ \st ->
    let msocket = HashMap.lookup socketId st
     in (msocket, HashMap.delete socketId st)

  -- shut down the handler thread
  mapM_ (Async.uninterruptibleCancel . handlerThread) msocket

toList :: (MonadIO m) => SocketStore -> m [Socket]
toList (SocketStore st) = do
  hm <- liftIO $ readTVarIO st
  return $ HashMap.elems hm
