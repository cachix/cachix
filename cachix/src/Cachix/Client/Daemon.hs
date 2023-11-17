module Cachix.Client.Daemon
  ( Types.Daemon,
    Types.runDaemon,
    new,
    start,
    run,
    stop,
    stopIO,
  )
where

import qualified Cachix.Client.Commands.Push as Commands.Push
import qualified Cachix.Client.Config as Config
import Cachix.Client.Config.Orphans ()
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
import qualified Control.Monad.Catch as E
import qualified Data.Text as T
import qualified Katip
import qualified Network.Socket as Socket
import Protolude
import System.Posix.Process (getProcessID)
import qualified UnliftIO.Async as Async
import qualified UnliftIO.QSem as QSem

-- | Configure a new daemon. Use 'run' to start it.
new ::
  -- | The Cachix environment.
  Env ->
  -- | Daemon-specific options.
  DaemonOptions ->
  -- | An optional handle to output logs to.
  Maybe Handle ->
  -- | Push options, like compression settings and number of jobs.
  PushOptions ->
  -- | The name of the binary cache to push to.
  BinaryCacheName ->
  -- | The configured daemon environment.
  IO DaemonEnv
new daemonEnv daemonOptions daemonLogHandle daemonPushOptions daemonCacheName = do
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

-- | Configure and run the daemon. Equivalent to running 'new' and 'run'.
start :: Env -> DaemonOptions -> PushOptions -> BinaryCacheName -> IO ()
start daemonEnv daemonOptions daemonPushOptions daemonCacheName = do
  daemon <- new daemonEnv daemonOptions Nothing daemonPushOptions daemonCacheName
  run daemon

-- | Run a daemon from a given configuration
run :: DaemonEnv -> IO ()
run daemon@DaemonEnv {..} = runDaemon daemon $ do
  Katip.logFM Katip.InfoS "Starting Cachix Daemon"

  config <- showConfiguration
  Katip.logFM Katip.InfoS $ Katip.ls $ T.intercalate "\n" ["Configuration:", config]

  let numWorkers = Options.numJobs daemonPushOptions

  Push.withPushParams $ \pushParams ->
    E.bracketOnError (Worker.startWorkers numWorkers (handleRequest pushParams)) Worker.stopWorkers $ \workers -> do
      flip E.onError shutdownQueue $
        -- TODO: retry the connection on socket errors
        E.bracketOnError (Daemon.openSocket daemonSocketPath) Daemon.closeSocket $ \sock -> do
          liftIO $ Socket.listen sock Socket.maxListenQueue

          res <-
            Async.race (waitForShutdown daemonShutdownLatch) $
              liftIO (Daemon.listen daemonQueue sock) `E.finally` stop

          waitForShutdown daemonShutdownLatch

          Katip.logFM Katip.InfoS "Shutting down daemon..."

          -- Stop receiving new push requests
          liftIO $ Socket.shutdown sock Socket.ShutdownReceive `catchAny` \_ -> return ()

          shutdownQueue

          -- Gracefully shutdown the worker *before* closing the socket
          Worker.stopWorkers workers

          -- TODO: say goodbye to all clients waiting for their push to go through
          case res of
            Right clientSock -> do
              -- Wave goodbye to the client that requested the shutdown
              liftIO $ Daemon.serverBye clientSock
              liftIO $ Socket.shutdown clientSock Socket.ShutdownBoth `catchAny` \_ -> return ()
            _ -> return ()

stop :: Daemon ()
stop = asks daemonShutdownLatch >>= initiateShutdown

stopIO :: DaemonEnv -> IO ()
stopIO DaemonEnv {daemonShutdownLatch} =
  initiateShutdown daemonShutdownLatch

shutdownQueue :: Daemon ()
shutdownQueue = do
  queue <- asks daemonQueue
  liftIO $ atomically $ closeTBMQueue queue

-- TODO: split into two jobs: 1. query/normalize/filter 2. push store path
handleRequest :: PushParams Daemon a -> QueuedPushRequest -> Daemon ()
handleRequest pushParams (QueuedPushRequest {..}) = do
  let store = pushParamsStore pushParams
  normalized <- mapM (Push.normalizeStorePath store) (Protocol.storePaths pushRequest)

  (allPaths, missingPaths) <- getMissingPathsForClosure pushParams (catMaybes normalized)

  paths <- pushOnClosureAttempt pushParams allPaths missingPaths

  qs <- asks daemonPushSemaphore
  let upload storePath =
        E.bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) $
          retryAll $
            uploadStorePath pushParams storePath

  Async.mapConcurrently_ upload paths

-- | Print debug information about the daemon configuration
showConfiguration :: Daemon Text
showConfiguration = do
  DaemonEnv {..} <- ask
  pure $
    T.intercalate
      "\n"
      [ "PID: " <> show daemonPid,
        "Socket: " <> toS daemonSocketPath,
        "Workers: " <> show (Options.numJobs daemonPushOptions),
        "Cache name: " <> toS daemonCacheName,
        "Cache URI: " <> BinaryCache.uri daemonBinaryCache,
        "Cache public keys: " <> show (BinaryCache.publicSigningKeys daemonBinaryCache),
        "Cache is public: " <> show (BinaryCache.isPublic daemonBinaryCache),
        "Compression: " <> show (Push.getCompressionMethod daemonPushOptions daemonBinaryCache)
      ]
