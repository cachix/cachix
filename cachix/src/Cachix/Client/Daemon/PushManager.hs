module Cachix.Client.Daemon.PushManager where

import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Types.Log (Logger)
import Cachix.Client.Daemon.Types.PushEvent
import Cachix.Client.Daemon.Types.PushManager
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TVar
import Control.Retry (RetryStatus)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Protolude

newPushManagerEnv :: Logger -> OnPushEvent -> IO PushManagerEnv
newPushManagerEnv pmLogger pmOnPushEvent = do
  pmPushJobs <- newTVarIO mempty
  pmStorePathToPushIds <- newTVarIO mempty
  pmActiveStorePaths <- newTVarIO mempty
  pmTaskQueue <- newTBMQueueIO 1000
  return $ PushManagerEnv {..}

runPushManager :: PushManagerEnv -> PushManager a -> IO a
runPushManager env f = unPushManager f `runReaderT` env

stopPushManager :: PushManagerEnv -> IO ()
stopPushManager PushManagerEnv {pmTaskQueue} =
  atomically $ closeTBMQueue pmTaskQueue

addPushJob :: (MonadIO m) => PushManagerEnv -> Protocol.PushRequest -> m ()
addPushJob PushManagerEnv {..} pushRequest = do
  pushJob <- newPushJob pushRequest
  liftIO $
    atomically $ do
      modifyTVar' pmPushJobs $ Map.insert (pushId pushJob) pushJob
      writeTBMQueue pmTaskQueue $ ResolveClosure (pushId pushJob)

lookupPushJob :: (MonadIO m) => PushManagerEnv -> Protocol.PushRequestId -> m (Maybe PushJob)
lookupPushJob PushManagerEnv {..} pushId = do
  liftIO $ Map.lookup pushId <$> readTVarIO pmPushJobs

queueStorePath :: (MonadIO m) => PushManagerEnv -> FilePath -> m ()
queueStorePath PushManagerEnv {..} storePath = do
  liftIO $ atomically $ do
    isDuplicate <- Set.member storePath <$> readTVar pmActiveStorePaths
    unless isDuplicate $ do
      writeTBMQueue pmTaskQueue (PushStorePath storePath)
      modifyTVar' pmActiveStorePaths $ Set.insert storePath

removeStorePath :: FilePath -> PushManager ()
removeStorePath storePath = do
  pmActiveStorePaths <- asks pmActiveStorePaths
  pmStorePathToPushIds <- asks pmStorePathToPushIds
  liftIO $ atomically $ do
    modifyTVar' pmActiveStorePaths $ Set.delete storePath
    modifyTVar' pmStorePathToPushIds $ Map.delete storePath

-- lookupPushIdsForStorePath :: FilePath -> STM [Protocol.PushRequestId]
lookupPushIdsForStorePath storePath pushIdStore =
  pure $ fromMaybe [] $ Map.lookup storePath pushIdStore

-- Events

pushStarted :: Protocol.PushRequestId -> PushManager ()
pushStarted pushId = return ()

-- pmEventChan <- asks pmOnPushEvent

-- liftIO $ do
--   timestamp <- getCurrentTime
--   pmEventChan pushId (PushStarted timestamp)

pushFinished :: Protocol.PushRequestId -> PushManager ()
pushFinished pushId = return ()

-- pmEventChan <- asks pmEventChan
-- liftIO $ do
--   timestamp <- getCurrentTime
--   atomically $ writeTMChan pmEventChan $ PushEvent timestamp pushId (PushFinished timestamp)

pushStorePathAttempt :: FilePath -> Int64 -> RetryStatus -> PushManager ()
pushStorePathAttempt storePath size retryStatus = do
  let pushRetryStatus = newPushRetryStatus retryStatus
  timestamp <- liftIO getCurrentTime
  pmStorePathToPushIds <- asks pmStorePathToPushIds
  sendPushEvent <- asks pmOnPushEvent
  pushIds <- liftIO $ do
    pushIdStore <- readTVarIO pmStorePathToPushIds
    lookupPushIdsForStorePath (toS storePath) pushIdStore
  liftIO $ for_ pushIds $ \pushId ->
    sendPushEvent pushId $ PushEvent timestamp pushId (PushStorePathAttempt storePath size pushRetryStatus)

pushStorePathProgress :: FilePath -> Int64 -> Int64 -> PushManager ()
pushStorePathProgress storePath currentBytes newBytes = return ()

-- timestamp <- liftIO getCurrentTime
-- pmEventChan <- asks pmEventChan
-- pmStorePathToPushIds <- asks pmStorePathToPushIds
-- liftIO $
--   atomically $ do
--     pushIdStore <- readTVar pmStorePathToPushIds
--     pushIds <- lookupPushIdsForStorePath (toS storePath) pushIdStore
--     for_ pushIds $ \pushId ->
--       writeTMChan pmEventChan $ PushEvent timestamp pushId (PushStorePathProgress storePath currentBytes newBytes)

pushStorePathDone :: FilePath -> PushManager ()
pushStorePathDone storePath = do
  timestamp <- liftIO getCurrentTime
  pmStorePathToPushIds <- asks pmStorePathToPushIds
  sendPushEvent <- asks pmOnPushEvent

  pushIds <- liftIO $ do
    pushIdStore <- readTVarIO pmStorePathToPushIds
    lookupPushIdsForStorePath (toS storePath) pushIdStore
  for_ pushIds $ \pushId ->
    liftIO $ sendPushEvent pushId $ PushEvent timestamp pushId (PushStorePathDone storePath)

  removeStorePath storePath

pushStorePathFailed :: FilePath -> Text -> PushManager ()
pushStorePathFailed storePath errMsg = return ()

newPushJob :: (MonadIO m) => Protocol.PushRequest -> m PushJob
newPushJob pushRequest = do
  pushId <- Protocol.newPushRequestId
  pushCreatedAt <- liftIO getCurrentTime
  let pushDetails = newPushDetails
  return $ PushJob {..}

newPushDetails :: PushDetails
newPushDetails =
  PushDetails
    { pdAllPaths = mempty,
      pdQueuedPaths = mempty,
      pdPushedPaths = mempty,
      pdFailedPaths = mempty,
      pdSkippedPaths = mempty
    }
