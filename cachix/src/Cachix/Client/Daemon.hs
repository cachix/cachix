module Cachix.Client.Daemon
  ( Types.Daemon,
    Types.runDaemon,
    new,
    start,
    run,
    stop,
    stopIO,
    subscribe,
  )
where

import qualified Cachix.Client.Commands.Push as Commands.Push
import qualified Cachix.Client.Config as Config
import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Listen as Daemon
import qualified Cachix.Client.Daemon.Log as Log
import Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Push as Push
import qualified Cachix.Client.Daemon.PushManager as PushManager
import Cachix.Client.Daemon.ShutdownLatch
import Cachix.Client.Daemon.Subscription as Subscription
import Cachix.Client.Daemon.Types as Types
import qualified Cachix.Client.Daemon.Types.PushManager as PushManager
import qualified Cachix.Client.Daemon.Worker as Worker
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions, PushOptions)
import qualified Cachix.Client.OptionsParser as Options
import Cachix.Client.Push
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Cachix.Types.BinaryCache as BinaryCache
import Control.Concurrent.STM.TMChan
import Control.Exception.Safe (catchAny)
import qualified Control.Monad.Catch as E
import qualified Data.Text as T
import qualified Hercules.CNix.Util as CNix.Util
import qualified Katip
import qualified Network.Socket as Socket
import Protolude
import System.Posix.Process (getProcessID)
import qualified System.Posix.Signals as Signal
import qualified UnliftIO.Async as Async

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
  let daemonLogLevel =
        if Config.verbose (Env.cachixoptions daemonEnv)
          then Debug
          else Info
  daemonLogger <- Log.new "cachix.daemon" daemonLogHandle daemonLogLevel

  daemonSocketPath <- maybe getSocketPath pure (Options.daemonSocketPath daemonOptions)
  daemonShutdownLatch <- newShutdownLatch
  daemonPid <- getProcessID

  daemonPushSecret <- Commands.Push.getPushSecretRequired (config daemonEnv) daemonCacheName
  let authToken = getAuthTokenFromPushSecret daemonPushSecret
  daemonBinaryCache <- Push.getBinaryCache daemonEnv authToken daemonCacheName

  daemonSubscriptionManager <- Subscription.newSubscriptionManager
  let onPushEvent = Subscription.pushEvent daemonSubscriptionManager
  daemonPushManager <- PushManager.newPushManagerEnv daemonPushOptions daemonLogger onPushEvent

  return $ DaemonEnv {..}

-- | Configure and run the daemon. Equivalent to running 'new' and 'run' together with some signal handling.
start :: Env -> DaemonOptions -> PushOptions -> BinaryCacheName -> IO ()
start daemonEnv daemonOptions daemonPushOptions daemonCacheName = do
  daemon <- new daemonEnv daemonOptions Nothing daemonPushOptions daemonCacheName
  installSignalHandlers daemon
  void $ run daemon

-- | Run a daemon from a given configuration
run :: DaemonEnv -> IO ExitCode
run daemon = runDaemon daemon $ flip E.onError (return $ ExitFailure 1) $ do
  Katip.logFM Katip.InfoS "Starting Cachix Daemon"
  DaemonEnv {..} <- ask

  config <- showConfiguration
  Katip.logFM Katip.InfoS $ Katip.ls $ "Configuration:\n" <> config

  let workerCount = Options.numJobs daemonPushOptions
      startWorkers pushParams =
        Worker.startWorkers
          workerCount
          (PushManager.pmTaskQueue daemonPushManager)
          (liftIO . PushManager.runPushManager daemonPushManager . PushManager.handleTask pushParams)

  subscriptionManagerThread <-
    liftIO $ Async.async $ runSubscriptionManager daemonSubscriptionManager

  let stopPushManager =
        liftIO $ PushManager.stopPushManager daemonPushManager

  Push.withPushParams $ \pushParams ->
    E.bracketOnError (startWorkers pushParams) Worker.stopWorkers $ \workers -> do
      flip E.onException stopPushManager $
        -- TODO: retry the connection on socket errors
        E.bracketOnError (Daemon.openSocket daemonSocketPath) Daemon.closeSocket $ \sock -> do
          liftIO $ Socket.listen sock Socket.maxListenQueue

          listenThread <- Async.async $ Daemon.listen stop queueJob sock

          waitForShutdown daemonShutdownLatch

          Katip.logFM Katip.InfoS "Shutting down daemon..."

          queuedStorePathCount <- PushManager.runPushManager daemonPushManager PushManager.queuedStorePathCount
          when (queuedStorePathCount > 0) $
            Katip.logFM Katip.InfoS $
              Katip.logStr $
                "Remaining store paths: " <> (show queuedStorePathCount :: Text)

          -- Stop receiving new push requests
          liftIO $ Socket.shutdown sock Socket.ShutdownReceive `catchAny` \_ -> return ()

          stopPushManager

          -- Gracefully shutdown the worker *before* closing the socket
          Worker.stopWorkers workers

          liftIO $ stopSubscriptionManager daemonSubscriptionManager
          Async.wait subscriptionManagerThread

          -- TODO: say goodbye to all clients waiting for their push to go through
          Async.cancel listenThread
          res <- Async.waitCatch listenThread
          case res of
            Right clientSock -> do
              -- Wave goodbye to the client that requested the shutdown
              liftIO $ Daemon.serverBye clientSock
              liftIO $ Socket.shutdown clientSock Socket.ShutdownBoth `catchAny` \_ -> return ()
            _ -> return ()

          return ExitSuccess

stop :: Daemon ()
stop = asks daemonShutdownLatch >>= initiateShutdown

stopIO :: DaemonEnv -> IO ()
stopIO DaemonEnv {daemonShutdownLatch} =
  initiateShutdown daemonShutdownLatch

installSignalHandlers :: DaemonEnv -> IO ()
installSignalHandlers daemon = do
  for_ [Signal.sigTERM, Signal.sigINT] $ \signal ->
    Signal.installHandler signal (Signal.CatchOnce handler) Nothing
  where
    handler = do
      CNix.Util.triggerInterrupt
      stopIO daemon

queueJob :: Protocol.PushRequest -> Socket.Socket -> Daemon ()
queueJob pushRequest _clientConn = do
  DaemonEnv {..} <- ask
  -- TODO: subscribe the socket to updates if requested

  -- Queue the job
  void $
    PushManager.runPushManager daemonPushManager (PushManager.addPushJob pushRequest)

subscribe :: DaemonEnv -> IO (TMChan PushEvent)
subscribe DaemonEnv {..} = do
  chan <- liftIO newBroadcastTMChanIO
  liftIO $ atomically $ do
    subscribeToAllSTM daemonSubscriptionManager (SubChannel chan)
    dupTMChan chan

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
