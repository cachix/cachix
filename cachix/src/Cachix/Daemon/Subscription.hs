module Cachix.Daemon.Subscription
  ( SubscriptionManager (..),
    Subscription (..),
    newSubscriptionManager,
    subscribeTo,
    subscribeToAll,
    subscribeToSTM,
    subscribeToAllSTM,
    queueUnsubscribe,
    getSubscriptionsFor,
    getSubscriptionsForSTM,
    pushEvent,
    pushEventSTM,
    runSubscriptionManager,
    stopSubscriptionManager,
  )
where

import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TMChan
import Control.Concurrent.STM.TVar
import Data.Aeson as Aeson (ToJSON)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Sequence qualified as Seq
import Network.Socket qualified as Socket
import Protolude

data SubscriptionManager k v = SubscriptionManager
  { managerSubscriptions :: TVar (HashMap k (Seq.Seq (Subscription v))),
    managerGlobalSubscriptions :: TVar (Seq.Seq (Subscription v)),
    managerMessages :: TBMQueue (ManagerMessage k v)
  }

data ManagerMessage k v
  = -- | An event to be dispatched to subscribers
    EventMessage k v
  | -- | Close subscriptions for a key (processed in order with events)
    UnsubscribeMessage k

data Subscription v
  = -- | A subscriber that listens on a socket.
    SubSocket (TBMQueue v) Socket.Socket
  | -- | A subscriber that listens on a channel.
    SubChannel (TMChan v)

newSubscriptionManager :: IO (SubscriptionManager k v)
newSubscriptionManager = do
  subscriptions <- newTVarIO HashMap.empty
  globalSubscriptions <- newTVarIO Seq.empty
  messages <- newTBMQueueIO 10000
  pure $ SubscriptionManager subscriptions globalSubscriptions messages

-- Subscriptions

subscribeTo :: (Hashable k, MonadIO m) => SubscriptionManager k v -> k -> Subscription v -> m ()
subscribeTo manager key subscription =
  liftIO $ atomically $ subscribeToSTM manager key subscription

subscribeToAll :: (MonadIO m) => SubscriptionManager k v -> Subscription v -> m ()
subscribeToAll manager subscription =
  liftIO $ atomically $ subscribeToAllSTM manager subscription

getSubscriptionsFor :: (Hashable k, MonadIO m) => SubscriptionManager k v -> k -> m (Seq.Seq (Subscription v))
getSubscriptionsFor manager key =
  liftIO $ atomically $ getSubscriptionsForSTM manager key

subscribeToSTM :: (Hashable k) => SubscriptionManager k v -> k -> Subscription v -> STM ()
subscribeToSTM manager key subscription = do
  subscriptions <- readTVar $ managerSubscriptions manager
  let subscriptions' = HashMap.insertWith (<>) key (Seq.singleton subscription) subscriptions
  writeTVar (managerSubscriptions manager) subscriptions'

subscribeToAllSTM :: SubscriptionManager k v -> Subscription v -> STM ()
subscribeToAllSTM manager subscription = do
  subscriptions <- readTVar $ managerGlobalSubscriptions manager
  let subscriptions' = subscription Seq.<| subscriptions
  writeTVar (managerGlobalSubscriptions manager) subscriptions'

getSubscriptionsForSTM :: (Hashable k) => SubscriptionManager k v -> k -> STM (Seq.Seq (Subscription v))
getSubscriptionsForSTM manager key = do
  subscriptions <- readTVar $ managerSubscriptions manager
  pure $ HashMap.lookupDefault Seq.empty key subscriptions

-- | Queue an unsubscribe command to close all subscription channels for a key.
-- This is processed in order with events, ensuring pending events are delivered first.
queueUnsubscribe :: (MonadIO m) => SubscriptionManager k v -> k -> m ()
queueUnsubscribe manager key =
  liftIO $ atomically $ writeTBMQueue (managerMessages manager) (UnsubscribeMessage key)

-- Events

pushEvent :: (MonadIO m) => SubscriptionManager k v -> k -> v -> m ()
pushEvent manager key event =
  liftIO $ atomically $ pushEventSTM manager key event

pushEventSTM :: SubscriptionManager k v -> k -> v -> STM ()
pushEventSTM manager key event =
  writeTBMQueue (managerMessages manager) (EventMessage key event)

sendEventToSub :: Subscription v -> v -> STM ()
-- TODO: implement socket subscriptions.
sendEventToSub (SubSocket _queue _) _ = pure () -- writeTBMQueue queue
sendEventToSub (SubChannel chan) event = writeTMChan chan event

runSubscriptionManager :: (Show k, Show v, Hashable k, ToJSON v, MonadIO m) => SubscriptionManager k v -> m ()
runSubscriptionManager manager = do
  isDone <- liftIO $ atomically $ do
    mmsg <- readTBMQueue (managerMessages manager)
    case mmsg of
      Nothing -> return True
      Just (EventMessage key event) -> do
        subscriptions <- getSubscriptionsForSTM manager key
        globalSubscriptions <- readTVar $ managerGlobalSubscriptions manager
        mapM_ (`sendEventToSub` event) (subscriptions <> globalSubscriptions)
        return False
      Just (UnsubscribeMessage key) -> do
        subscriptions <- readTVar $ managerSubscriptions manager
        case HashMap.lookup key subscriptions of
          Nothing -> return ()
          Just subs -> do
            forM_ subs $ \case
              SubSocket queue _ -> closeTBMQueue queue
              SubChannel chan -> closeTMChan chan
            writeTVar (managerSubscriptions manager) $ HashMap.delete key subscriptions
        return False

  unless isDone $
    runSubscriptionManager manager

stopSubscriptionManager :: SubscriptionManager k v -> IO ()
stopSubscriptionManager manager = do
  liftIO $ atomically $ closeTBMQueue (managerMessages manager)
  globalSubscriptions <- liftIO $ readTVarIO $ managerGlobalSubscriptions manager
  subscriptions <- liftIO $ readTVarIO $ managerSubscriptions manager

  forM_ (fold subscriptions <> globalSubscriptions) $ \case
    SubSocket queue sock -> do
      atomically $ closeTBMQueue queue
      Socket.close sock
    SubChannel channel -> atomically $ closeTMChan channel
