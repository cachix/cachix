module Cachix.Daemon.Subscription where

import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TVar
import Data.Aeson as Aeson (ToJSON, encode)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Network.Socket qualified as Socket
import Protolude
import Network.Socket.ByteString.Lazy qualified as Socket.LBS
import Cachix.Daemon.Protocol as Protocol
import Cachix.Daemon.Types.PushEvent
import Cachix.Daemon.Protocol (DaemonMessage(..))
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TBMQueue (writeTBMQueue)
import Control.Concurrent.STM.TMChan (writeTMChan)
import Control.Exception (catch, SomeException)

data SubscriptionManager k v = SubscriptionManager
  { managerSubscriptions :: TVar (HashMap k [Subscription v]),
    managerGlobalSubscriptions :: TVar [Subscription v],
    managerEvents :: TBMQueue (k, v)
  }

data Subscription v
  = -- | A subscriber that listens on a socket.
    SubSocket (TBMQueue v) Socket.Socket
  | -- | A subscriber that listens on a channel.
    SubChannel (TMChan v)

newSubscriptionManager :: IO (SubscriptionManager k v)
newSubscriptionManager = do
  subscriptions <- newTVarIO HashMap.empty
  globalSubscriptions <- newTVarIO []
  events <- newTBMQueueIO 10000
  pure $ SubscriptionManager subscriptions globalSubscriptions events

-- Subscriptions

subscribeTo :: (Hashable k, MonadIO m) => SubscriptionManager k v -> k -> Subscription v -> m ()
subscribeTo manager key subscription =
  liftIO $ atomically $ subscribeToSTM manager key subscription

subscribeToAll :: (MonadIO m) => SubscriptionManager k v -> Subscription v -> m ()
subscribeToAll manager subscription =
  liftIO $ atomically $ subscribeToAllSTM manager subscription

getSubscriptionsFor :: (Hashable k, MonadIO m) => SubscriptionManager k v -> k -> m [Subscription v]
getSubscriptionsFor manager key =
  liftIO $ atomically $ getSubscriptionsForSTM manager key

subscribeToSTM :: (Hashable k) => SubscriptionManager k v -> k -> Subscription v -> STM ()
subscribeToSTM manager key subscription = do
  subscriptions <- readTVar $ managerSubscriptions manager
  let subscriptions' = HashMap.insertWith (<>) key [subscription] subscriptions
  writeTVar (managerSubscriptions manager) subscriptions'

subscribeToAllSTM :: SubscriptionManager k v -> Subscription v -> STM ()
subscribeToAllSTM manager subscription = do
  subscriptions <- readTVar $ managerGlobalSubscriptions manager
  let subscriptions' = subscription : subscriptions
  writeTVar (managerGlobalSubscriptions manager) subscriptions'

getSubscriptionsForSTM :: (Hashable k) => SubscriptionManager k v -> k -> STM [Subscription v]
getSubscriptionsForSTM manager key = do
  subscriptions <- readTVar $ managerSubscriptions manager
  pure $ HashMap.lookupDefault [] key subscriptions

-- Events

pushEvent :: (MonadIO m) => SubscriptionManager k v -> k -> v -> m ()
pushEvent manager key event =
  liftIO $ atomically $ pushEventSTM manager key event

pushEventSTM :: SubscriptionManager k v -> k -> v -> STM ()
pushEventSTM manager key event =
  writeTBMQueue (managerEvents manager) (key, event)

sendEventToSub :: Subscription PushEvent -> PushEvent -> STM (IO ())
sendEventToSub (SubSocket queue sock) event = do
  writeTBMQueue queue event
  return $ sendEventToSubIO (SubSocket queue sock) event
sendEventToSub (SubChannel chan) event = do
  writeTMChan chan event
  return (return ()) -- No IO action needed for channels

sendEventToSubIO :: Subscription PushEvent -> PushEvent -> IO ()
sendEventToSubIO (SubSocket _ sock) event = do
  let msg = DaemonPushEvent event
  Socket.LBS.sendAll sock (encode msg) `catch` (\(_ :: SomeException) -> return ())
sendEventToSubIO (SubChannel _) _ = return ()

runSubscriptionManager :: (Show k, Hashable k, MonadIO m) => SubscriptionManager k PushEvent -> m ()
runSubscriptionManager manager = go
  where
    go = do
      mevent <- liftIO $ atomically $ readTBMQueue (managerEvents manager)
      case mevent of
        Nothing -> return ()
        Just (key, event) -> do
          subscriptions <- liftIO $ atomically $ getSubscriptionsForSTM manager key
          globalSubscriptions <- liftIO $ readTVarIO $ managerGlobalSubscriptions manager
          let actions = map (`sendEventToSub` event) (subscriptions <> globalSubscriptions)
          ioActions <- liftIO $ atomically $ sequence actions
          liftIO $ sequence_ ioActions
          go

stopSubscriptionManager :: SubscriptionManager k v -> IO ()
stopSubscriptionManager manager = do
  liftIO $ atomically $ closeTBMQueue (managerEvents manager)
  globalSubscriptions <- liftIO $ readTVarIO $ managerGlobalSubscriptions manager
  subscriptions <- liftIO $ readTVarIO $ managerSubscriptions manager

  forM_ (concat subscriptions <> globalSubscriptions) $ \case
    SubSocket queue sock -> do
      atomically $ closeTBMQueue queue
      Socket.close sock
    SubChannel channel -> atomically $ closeTMChan channel
