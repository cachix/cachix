{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.Daemon
  ( Daemon,
    new,
    start,
    run,
    push,
    stop,
    stopAndWait,
  )
where

import Cachix.Client.CNix (filterInvalidStorePath, followLinksToStorePath)
import qualified Cachix.Client.Commands.Push as Commands.Push
import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Client (push, stop, stopAndWait)
import Cachix.Client.Daemon.Listen as Daemon
import Cachix.Client.Daemon.Types as Types
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions, PushOptions)
import qualified Cachix.Client.OptionsParser as Options
import Cachix.Client.Push
import Cachix.Types.BinaryCache (BinaryCacheName)
import Control.Concurrent.Extra (once)
import Control.Concurrent.STM.TBMQueue
import Control.Exception.Safe (catchAny)
import qualified Control.Immortal as Immortal
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.String.Here (iTrim)
import qualified Data.Text as T
import qualified Network.Socket as Socket
import Protolude
import System.Posix.Process (getProcessID)
import qualified System.Posix.Signals as Signals

data Daemon = Daemon
  { -- | Cachix client env
    daemonEnv :: Env,
    -- | Push options, like compression settings
    daemonPushOptions :: PushOptions,
    -- | Path to the socket that the daemon listens on
    daemonSocketPath :: FilePath,
    -- | Queue of push requests to be processed by the worker thread
    daemonQueue :: TBMQueue QueuedPushRequest,
    -- | The binary cache to push to
    daemonCacheName :: BinaryCacheName,
    -- | The push secret for the binary cache
    daemonPushSecret :: PushSecret
  }

-- | Configure a new daemon. Use 'run' to start it.
new :: Env -> DaemonOptions -> PushOptions -> BinaryCacheName -> IO Daemon
new daemonEnv daemonOptions daemonPushOptions daemonCacheName = do
  daemonSocketPath <- maybe getSocketPath pure (Options.daemonSocketPath daemonOptions)
  daemonQueue <- newTBMQueueIO 1000
  daemonPushSecret <- Commands.Push.getPushSecretRequired (config daemonEnv) daemonCacheName
  return $ Daemon {..}

-- | Configure and run the daemon
start :: Env -> DaemonOptions -> PushOptions -> BinaryCacheName -> IO ()
start daemonEnv daemonOptions daemonPushOptions daemonCacheName = do
  daemon <- new daemonEnv daemonOptions daemonPushOptions daemonCacheName
  run daemon `finally` putErrText "Daemon shut down."

-- | Run a daemon from a given configuration
run :: Daemon -> IO ()
run daemon@Daemon {..} = do
  -- Ignore SIGPIPE errors
  _ <- Signals.installHandler Signals.sigPIPE Signals.Ignore Nothing

  bracketOnError startWorker identity $ \shutdownWorker -> do
    -- TODO: retry the connection on socket errors
    bracketOnError (Daemon.openSocket daemonSocketPath) Socket.close $ \sock -> do
      Socket.listen sock Socket.maxListenQueue

      putText =<< readyMessage daemonSocketPath daemonCacheName
      clientSock <- Daemon.listen daemonQueue sock

      -- Stop receiving new push requests
      Socket.shutdown sock Socket.ShutdownReceive `catchAny` \_ -> return ()

      -- Gracefully shutdown the worker before closing the socket
      shutdownWorker

      -- Wave goodbye to the client
      Daemon.serverBye clientSock

      Socket.shutdown clientSock Socket.ShutdownBoth `catchAny` \_ -> return ()
  where
    startWorker = do
      worker <- Immortal.create $ \thread ->
        Immortal.onUnexpectedFinish thread logWorkerException $
          runWorker daemon

      once $ do
        putErrText "Shutting down daemon..."
        atomically $ closeTBMQueue daemonQueue
        Immortal.mortalize worker
        putErrText "Waiting for worker to finish..."
        Immortal.wait worker
        putErrText "Worker finished."

logWorkerException :: (Exception e) => Either e () -> IO ()
logWorkerException (Left err) =
  putErrText $ "Exception in daemon worker thread: " <> toS (displayException err)
logWorkerException _ = return ()

runWorker :: Daemon -> IO ()
runWorker daemon@Daemon {..} = loop
  where
    loop =
      atomically (readTBMQueue daemonQueue) >>= \case
        Nothing -> return ()
        Just msg -> do
          handleRequest daemon daemonPushOptions msg
          loop

handleRequest :: Daemon -> PushOptions -> QueuedPushRequest -> IO ()
handleRequest Daemon {..} pushOptions (QueuedPushRequest {..}) =
  Commands.Push.withPushParams' daemonEnv pushOptions daemonCacheName daemonPushSecret $ \pushParams -> do
    normalized <-
      for (storePaths pushRequest) $ \fp -> runMaybeT $ do
        storePath <- MaybeT $ followLinksToStorePath (pushParamsStore pushParams) (encodeUtf8 $ T.pack fp)
        MaybeT $ filterInvalidStorePath (pushParamsStore pushParams) storePath

    void $
      pushClosure
        (mapConcurrentlyBounded (Options.numJobs pushOptions))
        pushParams
        (catMaybes normalized)

readyMessage :: FilePath -> BinaryCacheName -> IO Text
readyMessage socketPath cacheName = do
  -- Get the PID of the process
  pid <- getProcessID
  return
    [iTrim|
Cachix Daemon is ready to push store paths to ${cacheName}
PID: ${show pid :: Text}
Listening on socket: ${toS socketPath :: Text}
  |]
