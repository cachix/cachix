module Cachix.Client.Daemon.SocketStore
  ( newSocketStore,
    addSocket,
    removeSocket,
    toList,
    Socket (..),
  )
where

import Cachix.Client.Daemon.Types.SocketStore (Socket (..), SocketId, SocketStore (..))
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4 as UUID
import qualified Network.Socket
import Protolude hiding (toList)
import qualified UnliftIO.Async as Async

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
