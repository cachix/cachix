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
import qualified Cachix.Client.Config as Config
import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Client (push, stop, stopAndWait)
import Cachix.Client.Daemon.Listen as Daemon
import Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Push as Push
import Cachix.Client.Daemon.ShutdownLatch
import Cachix.Client.Daemon.Types as Types
import Cachix.Client.Daemon.Worker as Worker
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions, PushOptions)
import qualified Cachix.Client.OptionsParser as Options
import Cachix.Client.Push
import Cachix.Client.Retry (retryAll)
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Cachix.Types.BinaryCache as BinaryCache
import Control.Concurrent.STM.TBMQueue
import Control.Exception.Safe (catchAny)
import Control.Monad.Catch (bracketOnError, bracket_)
import qualified Katip
import qualified Network.Socket as Socket
import Protolude hiding (bracketOnError, bracket_)
import System.Posix.Process (getProcessID)
import qualified System.Posix.Signals as Signals
import qualified UnliftIO.Async as Async
import qualified UnliftIO.QSem as QSem

-- | Configure a new daemon. Use 'run' to start it.
new :: Env -> DaemonOptions -> PushOptions -> BinaryCacheName -> IO DaemonEnv
new daemonEnv daemonOptions daemonPushOptions daemonCacheName = do
  daemonSocketPath <- maybe getSocketPath pure (Options.daemonSocketPath daemonOptions)
  daemonQueue <- newTBMQueueIO 1000
  daemonShutdownLatch <- newShutdownLatch
  daemonPushSemaphore <- QSem.newQSem (Options.numJobs daemonPushOptions)
  daemonPid <- getProcessID
  daemonPushSecret <- Commands.Push.getPushSecretRequired (config daemonEnv) daemonCacheName

  let authToken = getAuthTokenFromPushSecret daemonPushSecret
  daemonBinaryCache <- Push.getBinaryCache daemonEnv authToken daemonCacheName

  let daemonLogLevel =
        if Config.verbose (Env.cachixoptions daemonEnv)
          then Debug
          else Info

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

  let numWorkers = Options.numJobs daemonPushOptions

  Push.withPushParams daemonEnv daemonPushOptions daemonBinaryCache daemonPushSecret $ \pushParams ->
    bracketOnError (Worker.startWorkers numWorkers (handleRequest pushParams)) Worker.stopWorkers $ \workers -> do
      -- TODO: retry the connection on socket errors
      bracketOnError (Daemon.openSocket daemonSocketPath) Daemon.closeSocket $ \sock -> do
        liftIO $ Socket.listen sock Socket.maxListenQueue

        clientSock <- liftIO $ Daemon.listen daemonQueue sock

        Katip.logFM Katip.InfoS "Received stop request from client"

        -- Stop receiving new push requests
        liftIO $ Socket.shutdown sock Socket.ShutdownReceive `catchAny` \_ -> return ()

        -- Gracefully shutdown the worker before closing the socket
        Worker.stopWorkers workers

        -- Wave goodbye to the client
        liftIO $ Daemon.serverBye clientSock

        liftIO $ Socket.shutdown clientSock Socket.ShutdownBoth `catchAny` \_ -> return ()

-- TODO: split into two jobs: 1. query/normalize/filter 2. push store path
handleRequest :: PushParams Daemon a -> QueuedPushRequest -> Daemon ()
handleRequest pushParams (QueuedPushRequest {..}) = do
  let store = pushParamsStore pushParams
  normalized <- mapM (Push.normalizeStorePath store) (Protocol.storePaths pushRequest)

  (allPaths, missingPaths) <- getMissingPathsForClosure pushParams (catMaybes normalized)

  paths <- pushOnClosureAttempt pushParams allPaths missingPaths

  qs <- asks daemonPushSemaphore
  let upload storePath =
        bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) $
          retryAll $
            uploadStorePath pushParams storePath

  Async.mapConcurrently_ upload paths

-- | Print debug information about the daemon configuration
showConfiguration :: Daemon Text
showConfiguration = do
  DaemonEnv {..} <- ask
  pure $
    unlines
      [ "PID: " <> show daemonPid,
        "Socket: " <> toS daemonSocketPath,
        "Workers: " <> show (Options.numJobs daemonPushOptions),
        "Cache name: " <> toS daemonCacheName,
        "Cache URI: " <> BinaryCache.uri daemonBinaryCache,
        "Cache public keys: " <> show (BinaryCache.publicSigningKeys daemonBinaryCache),
        "Cache is public: " <> show (BinaryCache.isPublic daemonBinaryCache),
        "Compression: " <> show (Push.getCompressionMethod daemonPushOptions daemonBinaryCache)
      ]
