module Cachix.Daemon.SocketStore
  ( newSocketStore,
    addSocket,
    removeSocket,
    addPublisherThread,
    cancelPublisherThread,
    toList,
    Socket (..),
    sendAll,
  )
where

import Cachix.Daemon.Types.PushEvent (PushRequestId)
import Cachix.Daemon.Types.SocketStore (Socket (..), SocketId, SocketStore (..))
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HashMap
import Data.UUID.V4 qualified as UUID
import Network.Socket qualified
import Network.Socket.ByteString.Lazy qualified as Socket.LBS
import Protolude hiding (toList)
import UnliftIO.Async qualified as Async

newSocketStore :: (MonadIO m) => m SocketStore
newSocketStore = SocketStore <$> liftIO (newTVarIO mempty)

newSocketId :: (MonadIO m) => m SocketId
newSocketId = liftIO UUID.nextRandom

addSocket :: (MonadUnliftIO m) => SocketStore -> Network.Socket.Socket -> (SocketId -> Network.Socket.Socket -> m ()) -> m ()
addSocket (SocketStore st) socket handler = do
  socketId <- newSocketId
  sendLock <- liftIO $ newMVar ()
  handlerThread <- Async.async (handler socketId socket)
  publisherThreads <- liftIO $ newTVarIO HashMap.empty
  liftIO $ atomically $ modifyTVar' st $ HashMap.insert socketId (Socket {..})

removeSocket :: (MonadIO m) => SocketStore -> SocketId -> m ()
removeSocket (SocketStore stvar) socketId = do
  msocket <- liftIO $ atomically $ stateTVar stvar $ \st ->
    let msocket = HashMap.lookup socketId st
     in (msocket, HashMap.delete socketId st)

  for_ msocket $ \sock -> do
    -- Cancel all publisher threads for this socket
    publishers <- liftIO $ readTVarIO (publisherThreads sock)
    mapM_ Async.cancel (HashMap.elems publishers)
    -- Cancel handler thread
    Async.uninterruptibleCancel (handlerThread sock)

addPublisherThread :: (MonadIO m) => SocketStore -> SocketId -> PushRequestId -> Async () -> m ()
addPublisherThread (SocketStore stvar) socketId pushId thread = liftIO $ atomically $ do
  sockets <- readTVar stvar
  for_ (HashMap.lookup socketId sockets) $ \sock ->
    modifyTVar' (publisherThreads sock) $ HashMap.insert pushId thread

cancelPublisherThread :: (MonadIO m) => SocketStore -> SocketId -> PushRequestId -> m ()
cancelPublisherThread (SocketStore stvar) socketId pushId = do
  mthread <- liftIO $ atomically $ do
    sockets <- readTVar stvar
    case HashMap.lookup socketId sockets of
      Nothing -> return Nothing
      Just sock -> stateTVar (publisherThreads sock) $ \threads ->
        (HashMap.lookup pushId threads, HashMap.delete pushId threads)
  mapM_ Async.cancel mthread

toList :: (MonadIO m) => SocketStore -> m [Socket]
toList (SocketStore st) = do
  hm <- liftIO $ readTVarIO st
  return $ HashMap.elems hm

sendAll :: (MonadIO m) => SocketStore -> SocketId -> LBS.ByteString -> m ()
sendAll (SocketStore stvar) socketId msg = do
  mSocket <- liftIO $ readTVarIO stvar
  case HashMap.lookup socketId mSocket of
    Just (Socket {socket, sendLock}) -> liftIO $ withMVar sendLock $ \_ -> Socket.LBS.sendAll socket msg
    Nothing -> return ()
