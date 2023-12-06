{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cachix.Client.Daemon.Types.Daemon
  ( -- * Daemon
    DaemonEnv (..),
    Daemon,
    runDaemon,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Event (HasEvent (..))
import qualified Cachix.Client.Daemon.Log as Log
import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.ShutdownLatch (ShutdownLatch)
import Cachix.Client.Daemon.Subscription (SubscriptionManager)
import Cachix.Client.Daemon.Types.Log (LogLevel, Logger)
import Cachix.Client.Daemon.Types.PushEvent (PushEvent)
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (PushOptions)
import Cachix.Client.Push
import Cachix.Types.BinaryCache (BinaryCache, BinaryCacheName)
import qualified Control.Concurrent.QSem as QSem
import Control.Concurrent.STM.TBMQueue
import Control.Concurrent.STM.TVar
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Retry (RetryStatus (..))
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
    -- | Queue of worker tasks
    daemonWorkerQueue :: TBMQueue PushJob,
    -- | A multiplexer for push events.
    daemonSubscriptionManager :: SubscriptionManager Protocol.PushRequestId PushEvent,
    -- | Logging env
    daemonLogger :: Logger,
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
  getLogEnv = Log.getKatipLogEnv <$> asks daemonLogger
  localLogEnv f (Daemon m) = Daemon (local (\s -> s {daemonLogger = Log.localLogEnv f (daemonLogger s)}) m)

instance Katip.KatipContext Daemon where
  getKatipContext = Log.getKatipContext <$> asks daemonLogger
  localKatipContext f (Daemon m) = Daemon (local (\s -> s {daemonLogger = Log.localKatipContext f (daemonLogger s)}) m)

  getKatipNamespace = Log.getKatipNamespace <$> asks daemonLogger
  localKatipNamespace f (Daemon m) = Daemon (local (\s -> s {daemonLogger = Log.localKatipNamespace f (daemonLogger s)}) m)

-- | Run a pre-configured daemon.
runDaemon :: DaemonEnv -> Daemon a -> IO a
runDaemon env f = do
  Log.withLogger (daemonLogger env) $ \logger ->
    unDaemon f `runReaderT` env {daemonLogger = logger}
