module Cachix.Client.Daemon.PushManager where

import Cachix.Client.Daemon.Log
import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Types.Log (Logger)
import Cachix.Client.Daemon.Types.PushManager
import Control.Concurrent.STM.TVar
import Control.Retry (RetryStatus)
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import Protolude

newPushManagerEnv :: Logger -> IO PushManagerEnv
newPushManagerEnv pmLogger = do
  pmPushRequests <- newTVarIO mempty
  pmStorePaths <- newTVarIO mempty
  return $ PushManagerEnv {..}

runPushManager :: PushManagerEnv -> PushManager a -> IO a
runPushManager env f = unPushManager f `runReaderT` env

addPushJob :: Protocol.PushRequest -> PushManager ()
addPushJob pushRequest = do
  PushManagerEnv {..} <- ask
  pushJob <- newPushJob pushRequest
  liftIO $
    atomically $
      modifyTVar' pmPushRequests $
        Map.insert (pushId pushJob) pushJob

pushStarted :: Protocol.PushRequestId -> PushManager ()
pushStarted pushId = undefined

pushFinished :: Protocol.PushRequestId -> PushManager ()
pushFinished pushId = undefined

pushStorePathAttempt :: FilePath -> Int64 -> RetryStatus -> PushManager ()
pushStorePathAttempt storePath size retryStatus = undefined

pushStorePathProgress :: FilePath -> Int64 -> Int64 -> PushManager ()
pushStorePathProgress storePath currentBytes newBytes = undefined

pushStorePathDone :: FilePath -> PushManager ()
pushStorePathDone storePath = undefined

pushStorePathFailed :: FilePath -> Text -> PushManager ()
pushStorePathFailed storePath errMsg = undefined

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
      pdPushedPaths = mempty,
      pdQueuedPaths = mempty,
      pdFailedPaths = mempty,
      pdSkippedPaths = mempty
    }

isPushJobDone :: PushJob -> Bool
isPushJobDone PushJob {pushDetails} = undefined

modifyPushJob :: Protocol.PushRequestId -> (PushJob -> PushJob) -> PushManager ()
modifyPushJob pushId f = do
  PushManagerEnv {..} <- ask
  return ()
