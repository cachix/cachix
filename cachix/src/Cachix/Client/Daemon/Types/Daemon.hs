{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cachix.Client.Daemon.Types.Daemon
  ( -- * Daemon
    DaemonEnv (..),
    Daemon,
    runDaemon,
  )
where

import Cachix.Client.Config.Orphans ()
import qualified Cachix.Client.Daemon.Log as Log
import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.ShutdownLatch (ShutdownLatch)
import Cachix.Client.Daemon.Subscription (SubscriptionManager)
import Cachix.Client.Daemon.Types.Error (DaemonError (DaemonUnhandledException), UnhandledException (..))
import Cachix.Client.Daemon.Types.EventLoop (EventLoop)
import Cachix.Client.Daemon.Types.Log (Logger)
import Cachix.Client.Daemon.Types.PushEvent (PushEvent)
import Cachix.Client.Daemon.Types.PushManager (PushManager, PushManagerEnv (..))
import Cachix.Client.Daemon.Types.SocketStore (SocketStore)
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (PushOptions)
import Cachix.Client.Push
import Cachix.Types.BinaryCache (BinaryCache, BinaryCacheName)
import qualified Control.Exception.Safe as Safe
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Katip
import Protolude hiding (bracketOnError)
import System.Posix.Types (ProcessID)

data DaemonEnv = DaemonEnv
  { -- | Cachix client env
    daemonEnv :: Env,
    -- | The main event loop
    daemonEventLoop :: EventLoop (Either DaemonError ()),
    -- | Push options, like compression settings and number of jobs
    daemonPushOptions :: PushOptions,
    -- | Path to the socket that the daemon listens on
    daemonSocketPath :: FilePath,
    -- | Main inbound socket thread
    daemonSocketThread :: MVar (Async ()),
    -- | The name of the binary cache to push to
    daemonCacheName :: BinaryCacheName,
    -- | The binary cache to push to
    daemonBinaryCache :: BinaryCache,
    -- | The state of active push requests
    daemonPushManager :: PushManagerEnv,
    -- | Connected clients over the socket
    daemonClients :: SocketStore,
    -- | A multiplexer for push events.
    daemonSubscriptionManager :: SubscriptionManager Protocol.PushRequestId PushEvent,
    -- | Logging env
    daemonLogger :: Logger,
    -- | Shutdown latch
    daemonShutdownLatch :: ShutdownLatch,
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
runDaemon :: DaemonEnv -> Daemon a -> IO (Either DaemonError a)
runDaemon env f = tryDaemon $ do
  Log.withLogger (daemonLogger env) $ \logger -> do
    let pushManagerEnv = (daemonPushManager env) {pmLogger = logger}
    unDaemon f `runReaderT` env {daemonLogger = logger, daemonPushManager = pushManagerEnv}
  where
    tryDaemon :: IO a -> IO (Either DaemonError a)
    tryDaemon a =
      Safe.catch
        (a >>= \v -> return (Right v))
        (return . Left . wrapUnhandledErrors)

    -- Wrap errors in a DaemonError
    wrapUnhandledErrors :: SomeException -> DaemonError
    wrapUnhandledErrors e | Just daemonError <- fromException @DaemonError e = daemonError
    wrapUnhandledErrors e = DaemonUnhandledException (UnhandledException e)
