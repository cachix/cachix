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
import qualified Data.Text as T
import Hercules.CNix.Store (Store, withStore)
import qualified Hercules.CNix.Util as CNix.Util
import qualified Katip
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.BS
import Protolude hiding (bracket)
import System.IO.Error (isResourceVanishedError)
import System.Posix.Process (getProcessID)
import qualified System.Posix.Signals as Signal
import UnliftIO (MonadUnliftIO)
import qualified UnliftIO.Async as Async
import UnliftIO.Exception (bracket)

-- | Configure a new daemon. Use 'run' to start it.
new ::
  -- | The Cachix environment.
  Env ->
  -- | A handle to the Nix store.
  Store ->
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
new daemonEnv nixStore daemonOptions daemonLogHandle daemonPushOptions daemonCacheName = do
  let daemonLogLevel =
        if Config.verbose (Env.cachixoptions daemonEnv)
          then Debug
          else Info
  daemonLogger <- Log.new "cachix.daemon" daemonLogHandle daemonLogLevel

  daemonEventLoop <- EventLoop.new
  daemonShutdownLatch <- newShutdownLatch
  daemonPid <- getProcessID

  daemonSocketPath <- maybe getSocketPath pure (Options.daemonSocketPath daemonOptions)
  daemonSocketThread <- newEmptyMVar
  daemonClients <- SocketStore.newSocketStore

  daemonPushSecret <- Commands.Push.getPushSecretRequired (config daemonEnv) daemonCacheName
  let authToken = getAuthTokenFromPushSecret daemonPushSecret
  daemonBinaryCache <- Push.getBinaryCache daemonEnv authToken daemonCacheName

  daemonSubscriptionManagerThread <- newEmptyMVar
  daemonSubscriptionManager <- Subscription.newSubscriptionManager
  let onPushEvent = Subscription.pushEvent daemonSubscriptionManager

  let pushParams = Push.newPushParams nixStore (clientenv daemonEnv) daemonBinaryCache daemonPushSecret daemonPushOptions
  daemonPushManager <- PushManager.newPushManagerEnv daemonPushOptions pushParams onPushEvent daemonLogger

  daemonWorkerThreads <- newEmptyMVar

  return $ DaemonEnv {..}

-- | Configure and run the daemon as a CLI command.
-- Equivalent to running 'withStore', new', and 'run', together with some signal handling.
start :: Env -> DaemonOptions -> PushOptions -> BinaryCacheName -> IO ()
start daemonEnv daemonOptions daemonPushOptions daemonCacheName =
  withStore $ \store -> do
    daemon <- new daemonEnv store daemonOptions Nothing daemonPushOptions daemonCacheName
    installSignalHandlers daemon
    result <- run daemon
    exitWith (toExitCode result)

-- | Run a daemon from a given configuration.
run :: DaemonEnv -> IO (Either DaemonError ())
run daemon = fmap join <$> runDaemon daemon $ do
  Katip.logFM Katip.InfoS "Starting Cachix Daemon"
  DaemonEnv {..} <- ask

  printConfiguration

  Async.async (runSubscriptionManager daemonSubscriptionManager)
    >>= (liftIO . putMVar daemonSubscriptionManagerThread)

  Worker.startWorkers
    (Options.numJobs daemonPushOptions)
    (PushManager.pmTaskQueue daemonPushManager)
    (liftIO . PushManager.runPushManager daemonPushManager . PushManager.handleTask)
    >>= (liftIO . putMVar daemonWorkerThreads)

  Async.async (Daemon.listen daemonEventLoop daemonSocketPath)
    >>= (liftIO . putMVar daemonSocketThread)

  eventLoopRes <- EventLoop.run daemonEventLoop $ \case
    EventLoop.AddSocketClient conn ->
      SocketStore.addSocket conn (Daemon.handleClient daemonEventLoop) daemonClients
    EventLoop.RemoveSocketClient socketId ->
      SocketStore.removeSocket socketId daemonClients
    EventLoop.ReconnectSocket ->
      -- TODO: implement reconnection logic
      EventLoop.exitLoopWith (Left DaemonSocketError) daemonEventLoop
    EventLoop.ReceivedMessage clientMsg ->
      case clientMsg of
        ClientPushRequest pushRequest -> queueJob pushRequest
        ClientStop -> EventLoop.send daemonEventLoop EventLoop.ShutdownGracefully
        _ -> return ()
    EventLoop.ShutdownGracefully -> do
      Katip.logFM Katip.InfoS "Shutting down daemon..."
      pushResult <- shutdownGracefully
      Katip.logFM Katip.InfoS "Daemon shut down. Exiting."
      EventLoop.exitLoopWith pushResult daemonEventLoop

  return $ case eventLoopRes of
    Left err -> Left (DaemonEventLoopError err)
    Right (Left err) -> Left err
    Right (Right ()) -> Right ()

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

-- | Print the daemon configuration to the log.
printConfiguration :: Daemon ()
printConfiguration = do
  config <- showConfiguration
  Katip.logFM Katip.InfoS $ Katip.ls $ "Configuration:\n" <> config

-- | Fetch debug information about the daemon configuration.
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

shutdownGracefully :: Daemon (Either DaemonError ())
shutdownGracefully = do
  DaemonEnv {..} <- ask

  -- Indicate that the daemon is shutting down
  initiateShutdown daemonShutdownLatch

  -- Stop the push manager and wait for any remaining paths to be uploaded
  shutdownPushManager daemonPushManager

  -- Stop worker threads
  withTakeMVar daemonWorkerThreads Worker.stopWorkers

  -- Close all event subscriptions
  withTakeMVar daemonSubscriptionManagerThread (shutdownSubscriptions daemonSubscriptionManager)

  failedJobs <-
    PushManager.runPushManager daemonPushManager PushManager.getFailedPushJobs
  let pushResult =
        if null failedJobs
          then Right ()
          else Left DaemonPushFailure

  -- Gracefully close open connections to clients
  Async.mapConcurrently_ (sayGoodbye pushResult) =<< SocketStore.toList daemonClients

  return pushResult
  where
    shutdownPushManager daemonPushManager = do
      queuedStorePathCount <- PushManager.runPushManager daemonPushManager PushManager.queuedStorePathCount
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

    shutdownSubscriptions daemonSubscriptionManager subscriptionManagerThread = do
      Katip.logFM Katip.DebugS "Shutting down event manager..."
      liftIO $ stopSubscriptionManager daemonSubscriptionManager
      _ <- Async.wait subscriptionManagerThread
      Katip.logFM Katip.DebugS "Event manager shut down."

    sayGoodbye exitResult socket = do
      let clientSock = SocketStore.socket socket
      let clientThread = SocketStore.handlerThread socket
      Async.cancel clientThread

      -- Wave goodbye to the client that requested the shutdown
      liftIO $ Daemon.serverBye clientSock exitResult
      liftIO $ Socket.shutdown clientSock Socket.ShutdownBoth `catchAny` (\_ -> return ())
      -- Wait for the other end to disconnect
      ebs <- liftIO $ try $ Socket.BS.recv clientSock 4096
      case ebs of
        Left err | isResourceVanishedError err -> Katip.logFM Katip.DebugS "Client did not disconnect cleanly."
        Left err -> Katip.logFM Katip.DebugS $ Katip.ls $ "Client socket threw an error: " <> displayException err
        Right _ -> Katip.logFM Katip.DebugS "Client disconnected."

withTakeMVar :: (MonadUnliftIO m) => MVar a -> (a -> m ()) -> m ()
withTakeMVar mvar f = do
  bracket acquire release wrapper
  where
    acquire = liftIO $ tryTakeMVar mvar

    release Nothing = pure ()
    release (Just x) = liftIO $ putMVar mvar x

    wrapper Nothing = pure ()
    wrapper (Just x) = f x
