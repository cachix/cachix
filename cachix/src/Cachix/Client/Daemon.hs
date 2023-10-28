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

import qualified Cachix.Client.Commands.Push as Commands.Push
import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Client (push, stop, stopAndWait)
import Cachix.Client.Daemon.Listen as Daemon
import Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Push as Push
import Cachix.Client.Daemon.ShutdownLatch
import Cachix.Client.Daemon.Types as Types
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions, PushOptions)
import qualified Cachix.Client.OptionsParser as Options
import Cachix.Client.Push
import Cachix.Client.Retry (retryAll)
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Cachix.Types.BinaryCache as BinaryCache
import Control.Concurrent.STM.TBMQueue
import Control.Exception.Safe (catchAny)
import qualified Control.Immortal as Immortal
import Control.Monad.Catch (bracketOnError)
import qualified Katip
import qualified Network.Socket as Socket
import Protolude hiding (bracketOnError)
import System.Posix.Process (getProcessID)
import qualified System.Posix.Signals as Signals

-- | Configure a new daemon. Use 'run' to start it.
new :: Env -> DaemonOptions -> PushOptions -> BinaryCacheName -> IO DaemonEnv
new daemonEnv daemonOptions daemonPushOptions daemonCacheName = do
  daemonSocketPath <- maybe getSocketPath pure (Options.daemonSocketPath daemonOptions)
  daemonQueue <- newTBMQueueIO 1000
  daemonShutdownLatch <- newShutdownLatch
  daemonPid <- getProcessID
  daemonPushSecret <- Commands.Push.getPushSecretRequired (config daemonEnv) daemonCacheName

  let authToken = getAuthTokenFromPushSecret daemonPushSecret
  daemonBinaryCache <- Push.getBinaryCache daemonEnv authToken daemonCacheName

  daemonKLogEnv <- Katip.initLogEnv "cachix.daemon" ""
  let daemonKNamespace = mempty
  let daemonKContext = mempty

  return $ DaemonEnv {..}

-- | Configure and run the daemon
start :: Env -> DaemonOptions -> PushOptions -> BinaryCacheName -> IO ()
start daemonEnv daemonOptions daemonPushOptions daemonCacheName = do
  daemon <- new daemonEnv daemonOptions daemonPushOptions daemonCacheName
  run daemon

-- | Run a daemon from a given configuration
run :: DaemonEnv -> IO ()
run daemon@DaemonEnv {..} = runDaemon daemon $ do
  -- Ignore SIGPIPE errors
  _ <- liftIO $ Signals.installHandler Signals.sigPIPE Signals.Ignore Nothing

  Katip.logFM Katip.InfoS "Starting Cachix Daemon"

  config <- showConfiguration
  Katip.logFM Katip.InfoS . Katip.ls $ unlines ["Configuration:", config]

  bracketOnError startWorker shutdownWorker $ \worker -> do
    -- TODO: retry the connection on socket errors
    bracketOnError (Daemon.openSocket daemonSocketPath) Daemon.closeSocket $ \sock -> do
      liftIO $ Socket.listen sock Socket.maxListenQueue

      clientSock <- liftIO $ Daemon.listen daemonQueue sock

      Katip.logFM Katip.InfoS "Received stop request from client"

      -- Stop receiving new push requests
      liftIO $ Socket.shutdown sock Socket.ShutdownReceive `catchAny` \_ -> return ()

      -- Gracefully shutdown the worker before closing the socket
      shutdownWorker worker

      -- Wave goodbye to the client
      liftIO $ Daemon.serverBye clientSock

      liftIO $ Socket.shutdown clientSock Socket.ShutdownBoth `catchAny` \_ -> return ()
  where
    startWorker = do
      Immortal.create $ \thread ->
        Katip.katipAddNamespace "worker" $
          Push.withPushParams daemonEnv daemonPushOptions daemonCacheName daemonPushSecret $ \pushParams ->
            Immortal.onUnexpectedFinish thread logWorkerException (runWorker pushParams)

    shutdownWorker :: Immortal.Thread -> Daemon ()
    shutdownWorker worker = do
      alreadyShuttingDown <- isShuttingDown daemonShutdownLatch

      unless alreadyShuttingDown $ do
        initiateShutdown daemonShutdownLatch
        Katip.logFM Katip.InfoS "Shutting down daemon..."
        liftIO $ atomically $ closeTBMQueue daemonQueue
        liftIO $ Immortal.mortalize worker
        Katip.logFM Katip.InfoS "Waiting for worker to finish..."
        liftIO $ Immortal.wait worker
        Katip.logFM Katip.InfoS "Worker finished."

logWorkerException :: (Exception e) => Either e () -> Daemon ()
logWorkerException (Left err) =
  Katip.logFM Katip.ErrorS $ Katip.ls (toS $ displayException err :: Text)
logWorkerException _ = return ()

runWorker :: PushParams Daemon () -> Daemon ()
runWorker pushParams = loop
  where
    loop = do
      DaemonEnv {..} <- ask
      mres <- liftIO $ atomically (readTBMQueue daemonQueue)
      case mres of
        Nothing -> return ()
        Just msg -> do
          handleRequest pushParams msg
          loop

handleRequest :: PushParams Daemon () -> QueuedPushRequest -> Daemon ()
handleRequest pushParams (QueuedPushRequest {..}) = do
  let store = pushParamsStore pushParams
  normalized <- mapM (Push.normalizeStorePath store) (Protocol.storePaths pushRequest)

  (allPaths, missingPaths) <- getMissingPathsForClosure pushParams (catMaybes normalized)

  paths <- pushOnClosureAttempt pushParams allPaths missingPaths

  for_ paths $ \storePath ->
    retryAll $ uploadStorePath pushParams storePath

-- | Print debug information about the daemon configuration
showConfiguration :: Daemon Text
showConfiguration = do
  DaemonEnv {..} <- ask
  pure $
    unlines
      [ "PID: " <> show daemonPid,
        "Socket: " <> toS daemonSocketPath,
        "Cache name: " <> toS daemonCacheName,
        "Cache URI: " <> BinaryCache.uri daemonBinaryCache,
        "Cache public keys: " <> show (BinaryCache.publicSigningKeys daemonBinaryCache),
        "Cache is public: " <> show (BinaryCache.isPublic daemonBinaryCache),
        "Compression: " <> show (Push.getCompressionMethod daemonPushOptions daemonBinaryCache)
      ]
