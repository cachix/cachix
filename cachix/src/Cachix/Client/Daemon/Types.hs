{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cachix.Client.Daemon.Types where

import Cachix.Client.Config.Orphans ()
import qualified Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.ShutdownLatch (ShutdownLatch)
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (PushOptions)
import Cachix.Client.Push
import Cachix.Types.BinaryCache (BinaryCache, BinaryCacheName)
import qualified Control.Concurrent.QSem as QSem
import Control.Concurrent.STM.TBMQueue
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Katip
import qualified Network.Socket as Socket
import Protolude hiding (bracketOnError)
import System.Posix.Types (ProcessID)

data DaemonEnv = DaemonEnv
  { -- | Cachix client env
    daemonEnv :: Env,
    -- | Push options, like compression settings and number of jobs
    daemonPushOptions :: PushOptions,
    -- | Path to the socket that the daemon listens on
    daemonSocketPath :: FilePath,
    -- | Queue of push requests to be processed by the worker thread
    daemonQueue :: TBMQueue QueuedPushRequest,
    -- | The push secret for the binary cache
    daemonPushSecret :: PushSecret,
    -- | The name of the binary cache to push to
    daemonCacheName :: BinaryCacheName,
    -- | The binary cache to push to
    daemonBinaryCache :: BinaryCache,
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

-- | Run a pre-configured daemon.
runDaemon :: DaemonEnv -> Daemon a -> IO a
runDaemon env f = do
  let logLevel = toKatipLogLevel (daemonLogLevel env)
  let registerScribe = do
        scribeHandle <- Katip.mkHandleScribe Katip.ColorIfTerminal stdout (Katip.permitItem logLevel) Katip.V2
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
data QueuedPushRequest = QueuedPushRequest
  { -- | The original push request.
    pushRequest :: Protocol.PushRequest,
    -- | An open socket to the client that sent the push request.
    clientConnection :: Socket.Socket
  }
