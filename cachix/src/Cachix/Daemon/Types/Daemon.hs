{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cachix.Daemon.Types.Daemon
  ( -- * Daemon
    DaemonEvent (..),
    DaemonEnv (..),
    Daemon,
    runDaemon,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (PushOptions)
import Cachix.Daemon.Log qualified as Log
import Cachix.Daemon.Protocol qualified as Protocol
import Cachix.Daemon.Subscription (SubscriptionManager)
import Cachix.Daemon.Types.Error (DaemonError (DaemonUnhandledException), UnhandledException (..))
import Cachix.Daemon.Types.EventLoop (EventLoop)
import Cachix.Daemon.Types.Log (Logger)
import Cachix.Daemon.Types.PushEvent (PushEvent)
import Cachix.Daemon.Types.PushManager (PushManagerEnv (..))
import Cachix.Daemon.Types.SocketStore (SocketId, SocketStore)
import Cachix.Daemon.Worker qualified as Worker
import Cachix.Types.BinaryCache (BinaryCache, BinaryCacheName)
import Control.Exception.Safe qualified as Safe
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Katip qualified
import Network.Socket (Socket)
import Protolude hiding (bracketOnError)
import System.Posix.Types (ProcessID)

-- | Daemon events that are handled by the 'EventLoop'.
data DaemonEvent
  = -- | Shut down the daemon gracefully.
    ShutdownGracefully
  | -- | Re-establish the daemon socket
    ReconnectSocket
  | -- | Add a new client socket connection.
    AddSocketClient Socket
  | -- | Remove an existing client socket connection. For example, after it is closed.
    RemoveSocketClient SocketId
  | -- | Handle a new message from a client.
    ReceivedMessage Protocol.ClientMessage

data DaemonEnv = DaemonEnv
  { -- | Cachix client env
    daemonEnv :: Env,
    -- | The main event loop
    daemonEventLoop :: EventLoop DaemonEvent (Either DaemonError ()),
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
    -- | A pool of worker threads
    daemonWorkerThreads :: MVar [Worker.Thread],
    -- | A multiplexer for push events
    daemonSubscriptionManager :: SubscriptionManager Protocol.PushRequestId PushEvent,
    -- | A thread handle for the subscription manager
    daemonSubscriptionManagerThread :: MVar (Async ()),
    -- | Logging env
    daemonLogger :: Logger,
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
