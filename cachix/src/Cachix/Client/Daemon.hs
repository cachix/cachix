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
import qualified Cachix.Client.Daemon.EventLoop as EventLoop
import Cachix.Client.Daemon.Listen as Daemon
import qualified Cachix.Client.Daemon.Log as Log
import Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Push as Push
import qualified Cachix.Client.Daemon.PushManager as PushManager
import Cachix.Client.Daemon.ShutdownLatch
import qualified Cachix.Client.Daemon.SocketStore as SocketStore
import Cachix.Client.Daemon.Subscription as Subscription
import Cachix.Client.Daemon.Types as Types
import Cachix.Client.Daemon.Types.EventLoop (DaemonEvent (ShutdownGracefully))
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
import qualified Network.Socket.ByteString as Socket.BS
import Protolude
import System.IO.Error (isResourceVanishedError)
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
  daemonSocketThread <- newEmptyMVar
  daemonEventLoop <- EventLoop.new
  daemonClients <- SocketStore.newSocketStore
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

  Push.withPushParams $ \pushParams -> do
    subscriptionManagerThread <-
      Async.async $ runSubscriptionManager daemonSubscriptionManager

    let runWorkerTask =
          liftIO . PushManager.runPushManager daemonPushManager . PushManager.handleTask pushParams
    workersThreads <-
      Worker.startWorkers
        (Options.numJobs daemonPushOptions)
        (PushManager.pmTaskQueue daemonPushManager)
        runWorkerTask

    listenThread <- Async.async (Daemon.listen daemonEventLoop daemonSocketPath)
    liftIO $ putMVar daemonSocketThread listenThread

    EventLoop.run daemonEventLoop $ \case
      EventLoop.AddSocketClient conn ->
        SocketStore.addSocket conn (Daemon.handleClient daemonEventLoop) daemonClients
      EventLoop.RemoveSocketClient socketId ->
        SocketStore.removeSocket socketId daemonClients
      EventLoop.ReconnectSocket ->
        -- TODO: implement reconnection logic
        EventLoop.exitLoopWith (ExitFailure 1) daemonEventLoop
      EventLoop.ReceivedMessage clientMsg ->
        case clientMsg of
          ClientPushRequest pushRequest -> queueJob pushRequest
          ClientStop -> EventLoop.send daemonEventLoop EventLoop.ShutdownGracefully
          _ -> return ()
      EventLoop.ShutdownGracefully -> do
        Katip.logFM Katip.InfoS "Shutting down daemon..."
        shutdownGracefully subscriptionManagerThread workersThreads
        Katip.logFM Katip.InfoS "Daemon shut down. Exiting."
        EventLoop.exitLoopWith ExitSuccess daemonEventLoop

stop :: Daemon ()
stop = do
  eventloop <- asks daemonEventLoop
  EventLoop.send eventloop ShutdownGracefully

stopIO :: DaemonEnv -> IO ()
stopIO DaemonEnv {daemonEventLoop} =
  EventLoop.sendIO daemonEventLoop ShutdownGracefully

installSignalHandlers :: DaemonEnv -> IO ()
installSignalHandlers daemon = do
  for_ [Signal.sigTERM, Signal.sigINT] $ \signal ->
    Signal.installHandler signal (Signal.CatchOnce handler) Nothing
  where
    handler = do
      CNix.Util.triggerInterrupt
      stopIO daemon

queueJob :: Protocol.PushRequest -> Daemon ()
queueJob pushRequest = do
  daemonPushManager <- asks daemonPushManager
  -- TODO: subscribe the socket to updates if available

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

shutdownGracefully :: Async () -> [Worker.Thread] -> Daemon ()
shutdownGracefully subscriptionManagerThread workersThreads = do
  DaemonEnv {..} <- ask

  initiateShutdown daemonShutdownLatch
  PushManager.runPushManager daemonPushManager $ do
    queuedStorePathCount <- PushManager.queuedStorePathCount
    when (queuedStorePathCount > 0) $
      Katip.logFM Katip.InfoS $
        Katip.logStr $
          "Remaining store paths: " <> (show queuedStorePathCount :: Text)

  Katip.logFM Katip.DebugS "Waiting for push manager to clear remaining jobs..."
  -- Finish processing remaining push jobs
  let timeoutOptions =
        PushManager.TimeoutOptions
          { PushManager.toTimeout = 60.0,
            PushManager.toPollingInterval = 1.0
          }
  liftIO $ PushManager.stopPushManager timeoutOptions daemonPushManager
  Katip.logFM Katip.DebugS "Push manager shut down."

  -- Gracefully shut down the worker before closing the socket
  Worker.stopWorkers workersThreads

  -- Close all event subscriptions
  Katip.logFM Katip.DebugS "Shutting down event manager..."
  liftIO $ stopSubscriptionManager daemonSubscriptionManager
  Async.wait subscriptionManagerThread
  Katip.logFM Katip.DebugS "Event manager shut down."

  let sayGoodbye socket = do
        let clientSock = SocketStore.socket socket
        let clientThread = SocketStore.handlerThread socket
        Async.cancel clientThread

        -- Wave goodbye to the client that requested the shutdown
        liftIO $ Daemon.serverBye clientSock
        liftIO $ Socket.shutdown clientSock Socket.ShutdownBoth `catchAny` (\_ -> return ())
        -- Wait for the other end to disconnect
        ebs <- liftIO $ try $ Socket.BS.recv clientSock 4096
        case ebs of
          Left err | isResourceVanishedError err -> Katip.logFM Katip.DebugS "Client did not disconnect cleanly."
          Left err -> Katip.logFM Katip.DebugS $ Katip.ls $ "Client socket threw an error: " <> displayException err
          Right _ -> Katip.logFM Katip.DebugS "Client disconnected."

  Async.mapConcurrently_ sayGoodbye =<< SocketStore.toList daemonClients
