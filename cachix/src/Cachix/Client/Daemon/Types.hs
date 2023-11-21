{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cachix.Client.Daemon.Types where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Event as Event
import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.ShutdownLatch (ShutdownLatch)
import Cachix.Client.Daemon.Subscription (SubscriptionManager)
import qualified Cachix.Client.Daemon.Subscription as Subscription
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (PushOptions)
import Cachix.Client.Push
import Cachix.Types.BinaryCache (BinaryCache, BinaryCacheName)
import qualified Control.Concurrent.QSem as QSem
import Control.Concurrent.STM.TBMQueue
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime, getCurrentTime)
import qualified Katip
import Protolude hiding (bracketOnError)
import System.Posix.Types (ProcessID)

data DaemonEnv = DaemonEnv
  { -- | Cachix client env
    daemonEnv :: Env,
    -- | Push options, like compression settings and number of jobs
    daemonPushOptions :: PushOptions,
    -- | Path to the socket that the daemon listens on
    daemonSocketPath :: FilePath,
    -- | The push secret for the binary cache
    daemonPushSecret :: PushSecret,
    -- | The name of the binary cache to push to
    daemonCacheName :: BinaryCacheName,
    -- | The binary cache to push to
    daemonBinaryCache :: BinaryCache,
    -- | Queue of push requests to be processed by the worker threads
    daemonQueue :: TBMQueue PushJob,
    -- | A multiplexer for push events.
    daemonSubscriptionManager :: SubscriptionManager Protocol.PushRequestId PushEvent,
    -- | An optional handle to output logs to.
    -- Defaults to stdout.
    daemonLogHandle :: Maybe Handle,
    -- | The log level to use for logging
    daemonLogLevel :: LogLevel,
    -- | Logger namespace
    daemonKNamespace :: Katip.Namespace,
    -- | Logger context
    daemonKContext :: Katip.LogContexts,
    -- | Logger env
    daemonKLogEnv :: Katip.LogEnv,
    -- | Shutdown latch
    daemonShutdownLatch :: ShutdownLatch,
    -- | A semaphore to limit the number of concurrent pushes
    daemonPushSemaphore :: QSem.QSem,
    -- | The PID of the daemon process
    daemonPid :: ProcessID
  }

newtype Daemon a = Daemon
  { unDaemon :: ReaderT DaemonEnv IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader DaemonEnv,
      MonadUnliftIO,
      MonadCatch,
      MonadMask,
      MonadThrow
    )

instance Katip.Katip Daemon where
  getLogEnv = asks daemonKLogEnv
  localLogEnv f (Daemon m) = Daemon (local (\s -> s {daemonKLogEnv = f (daemonKLogEnv s)}) m)

instance Katip.KatipContext Daemon where
  getKatipContext = asks daemonKContext
  localKatipContext f (Daemon m) = Daemon (local (\s -> s {daemonKContext = f (daemonKContext s)}) m)

  getKatipNamespace = asks daemonKNamespace
  localKatipNamespace f (Daemon m) = Daemon (local (\s -> s {daemonKNamespace = f (daemonKNamespace s)}) m)

instance HasEvent Daemon where
  type Key Daemon = Protocol.PushRequestId
  type Event Daemon = PushEvent

  pushEvent k v = do
    daemonSubscriptionManager <- asks daemonSubscriptionManager
    Subscription.pushEvent daemonSubscriptionManager k v

  pushStarted pushId = do
    timestamp <- liftIO getCurrentTime
    pushEvent pushId $ PushEvent timestamp pushId (PushStarted timestamp)

  pushFinished pushId = do
    timestamp <- liftIO getCurrentTime
    pushEvent pushId $ PushEvent timestamp pushId (PushFinished timestamp)

  pushStorePathAttempt pushId storePath size = do
    timestamp <- liftIO getCurrentTime
    pushEvent pushId $ PushEvent timestamp pushId (PushStorePathAttempt storePath size)

  pushStorePathProgress pushId storePath progress = do
    timestamp <- liftIO getCurrentTime
    pushEvent pushId $ PushEvent timestamp pushId (PushStorePathProgress storePath progress)

  pushStorePathDone pushId storePath = do
    timestamp <- liftIO getCurrentTime
    pushEvent pushId $ PushEvent timestamp pushId (PushStorePathDone storePath)

  pushStorePathFailed pushId storePath errMsg = do
    timestamp <- liftIO getCurrentTime
    pushEvent pushId $ PushEvent timestamp pushId (PushStorePathFailed storePath errMsg)

-- | Run a pre-configured daemon.
runDaemon :: DaemonEnv -> Daemon a -> IO a
runDaemon env f = do
  let logLevel = toKatipLogLevel (daemonLogLevel env)
  let logHandle = fromMaybe stdout (daemonLogHandle env)
  let registerScribe = do
        scribeHandle <- Katip.mkHandleScribe Katip.ColorIfTerminal logHandle (Katip.permitItem logLevel) Katip.V2
        Katip.registerScribe "stdout" scribeHandle Katip.defaultScribeSettings (daemonKLogEnv env)

  bracket registerScribe Katip.closeScribes $ \logEnv -> do
    unDaemon f `runReaderT` env {daemonKLogEnv = logEnv}

-- | The log level to use for logging
--
-- TODO: reuse in deploy agent
data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving stock (Eq, Ord, Show)

toKatipLogLevel :: LogLevel -> Katip.Severity
toKatipLogLevel = \case
  Debug -> Katip.DebugS
  Info -> Katip.InfoS
  Warning -> Katip.WarningS
  Error -> Katip.ErrorS

-- | A push request that has been queued for processing.
data PushJob = PushJob
  { -- | A unique identifier for this push request.
    pushId :: Protocol.PushRequestId,
    -- | The time when the push request was queued.
    pushCreatedAt :: UTCTime,
    -- | The original push request.
    pushRequest :: Protocol.PushRequest
  }
  deriving (Show)

data PushEvent = PushEvent
  { eventTimestamp :: UTCTime,
    eventPushId :: Protocol.PushRequestId,
    eventMessage :: PushEventMessage
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Ord PushEvent where
  compare = compare `on` eventTimestamp

data PushEventMessage
  = PushStarted UTCTime
  | PushStorePathAttempt FilePath Int64
  | PushStorePathProgress FilePath Int64
  | PushStorePathDone FilePath
  | PushStorePathFailed FilePath Text
  | PushFinished UTCTime
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newPushJob :: (MonadIO m) => Protocol.PushRequest -> m PushJob
newPushJob pushRequest = do
  pushId <- Protocol.newPushRequestId
  pushCreatedAt <- liftIO getCurrentTime
  return $ PushJob {..}
