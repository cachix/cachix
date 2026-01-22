module Cachix.Daemon
  ( Types.Daemon,
    Types.runDaemon,
    new,
    start,
    run,
    stop,
    stopIO,
    subscribe,
    subscribeAll,
  )
where

import Cachix.API qualified as API
import Cachix.Client.Command.Push qualified as Command.Push
import Cachix.Client.Config qualified as Config
import Cachix.Client.Config.Orphans ()
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (DaemonOptions, PushOptions, daemonNarinfoQueryOptions)
import Cachix.Client.OptionsParser qualified as Options
import Cachix.Client.Push
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant (cachixClient)
import Cachix.Daemon.DryMode qualified as DryMode
import Cachix.Daemon.EventLoop qualified as EventLoop
import Cachix.Daemon.Listen as Listen
import Cachix.Daemon.Log qualified as Log
import Cachix.Daemon.NarinfoQuery qualified as NarinfoQuery
import Cachix.Daemon.Protocol as Protocol
import Cachix.Daemon.Push as Push
import Cachix.Daemon.PushManager qualified as PushManager
import Cachix.Daemon.SocketStore qualified as SocketStore
import Cachix.Daemon.Subscription as Subscription
import Cachix.Daemon.Types as Types
import Cachix.Daemon.Types.PushManager qualified as PushManager
import Cachix.Daemon.Types.SocketStore (SocketId)
import Cachix.Daemon.Worker qualified as Worker
import Cachix.Types.BinaryCache (BinaryCacheName)
import Cachix.Types.BinaryCache qualified as BinaryCache
import Control.Concurrent.STM.TMChan
import Control.Exception.Safe (catchAny)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text qualified as T
import Hercules.CNix.Store (Store, withStore)
import Hercules.CNix.Util qualified as CNix.Util
import Katip qualified
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.BS
import Protolude hiding (bracket)
import Servant.Client.Streaming (runClientM)
import System.Environment (lookupEnv)
import System.IO.Error (isResourceVanishedError)
import System.Posix.Process (getProcessID)
import System.Posix.Signals qualified as Signal
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Async qualified as Async
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
  daemonPid <- getProcessID

  envSocket <- fmap toS <$> lookupEnv "CACHIX_DAEMON_SOCKET"
  let cmdSocket = Options.daemonSocketPath daemonOptions
  daemonSocketPath <- maybe getSocketPath pure (envSocket <|> cmdSocket)

  daemonSocketThread <- newEmptyMVar
  daemonClients <- SocketStore.newSocketStore

  let isDryRun = Options.daemonDryRun daemonOptions

  -- In dry run, use mock implementations; otherwise fetch from API
  (daemonBinaryCache, daemonPushSecret) <-
    if isDryRun
      then pure (DryMode.mockBinaryCache daemonCacheName, DryMode.mockPushSecret)
      else do
        pushSecret <- Command.Push.getPushSecretRequired (config daemonEnv) daemonCacheName
        let authToken = getAuthTokenFromPushSecret pushSecret
        binaryCache <- Push.getBinaryCache daemonEnv authToken daemonCacheName
        pure (binaryCache, pushSecret)

  daemonSubscriptionManagerThread <- newEmptyMVar
  daemonSubscriptionManager <- Subscription.newSubscriptionManager
  let onPushEvent pushId event = do
        Subscription.pushEvent daemonSubscriptionManager pushId event
        case Types.eventMessage event of
          Types.PushFinished -> Subscription.queueUnsubscribe daemonSubscriptionManager pushId
          _ -> return ()

  -- Set dry run in narinfo query options
  let narinfoOptions = (daemonNarinfoQueryOptions daemonOptions) {NarinfoQuery.nqoDryRun = isDryRun}
  let pushParams = Push.newPushParams nixStore (clientenv daemonEnv) daemonBinaryCache daemonPushSecret daemonPushOptions
  daemonPushManager <- PushManager.newPushManagerEnv daemonPushOptions narinfoOptions pushParams onPushEvent daemonLogger

  daemonWorkerThreads <- newEmptyMVar

  return $ DaemonEnv {daemonOptions = daemonOptions, ..}

-- | Configure and run the daemon as a CLI command.
-- Equivalent to running 'withStore', new', and 'run', together with some signal handling.
start :: Env -> DaemonOptions -> PushOptions -> BinaryCacheName -> IO ()
start daemonEnv daemonOptions daemonPushOptions daemonCacheName =
  withStore $ \store -> do
    daemon <- new daemonEnv store daemonOptions Nothing daemonPushOptions daemonCacheName
    void $ runDaemon daemon installSignalHandlers
    result <- run daemon
    exitWith (toExitCode result)

-- | Run a daemon from a given configuration.
run :: DaemonEnv -> IO (Either DaemonError ())
run daemon = fmap join <$> runDaemon daemon $ do
  Katip.logFM Katip.InfoS "Starting Cachix Daemon"
  DaemonEnv {..} <- ask

  when (Options.daemonDryRun daemonOptions) $
    Katip.logFM Katip.WarningS "Running in dry run mode - no network requests will be made"

  printConfiguration

  Async.async (runSubscriptionManager daemonSubscriptionManager)
    >>= (liftIO . putMVar daemonSubscriptionManagerThread)

  -- Start the narinfo batch processor
  PushManager.startBatchProcessor daemonPushManager

  -- Double the number of workers to keep queues filled with jobs
  let numWorkers = Options.numJobs daemonPushOptions * 2
  Worker.startWorkers
    numWorkers
    (PushManager.pmTaskQueue daemonPushManager)
    (liftIO . PushManager.runPushManager daemonPushManager . PushManager.handleTask)
    >>= (liftIO . putMVar daemonWorkerThreads)

  Async.async (Listen.listen daemonEventLoop daemonSocketPath)
    >>= (liftIO . putMVar daemonSocketThread)

  eventLoopRes <- EventLoop.run daemonEventLoop $ \case
    AddSocketClient conn ->
      SocketStore.addSocket conn (Listen.handleClient daemonEventLoop) daemonClients
    RemoveSocketClient socketId ->
      SocketStore.removeSocket socketId daemonClients
    ReconnectSocket ->
      -- TODO: implement reconnection logic
      EventLoop.exitLoopWith (Left DaemonSocketError) daemonEventLoop
    ReceivedMessage socketId clientMsg ->
      case clientMsg of
        ClientPushRequest pushRequest -> do
          -- Create push job upfront so we can subscribe before queuing
          pushJob <- liftIO $ PushManager.newPushJob pushRequest
          let pushId = PushManager.pushId pushJob

          Katip.katipAddContext (Katip.sl "push_id" pushId) $ do
            when (Protocol.subscribeToUpdates pushRequest) $ do
              chan <- liftIO $ subscribe daemon (Just pushId)
              publisherThread <- liftIO $ Async.async $ publishToClient socketId chan daemon
              SocketStore.addPublisherThread socketId pushId publisherThread daemonClients

            -- Now add the job
            success <- queueJob pushJob
            unless success $
              Katip.logFM Katip.ErrorS "Failed to queue push request"
        ClientStop -> do
          DaemonEnv {daemonOptions = opts, daemonClients = clients} <- ask
          if Options.daemonAllowRemoteStop opts
            then EventLoop.send daemonEventLoop ShutdownGracefully
            else do
              let errorMsg = "Remote stop is disabled on this daemon"
              SocketStore.sendAll socketId (Protocol.newMessage (Protocol.DaemonError (Protocol.UnsupportedCommand errorMsg))) clients
        ClientDiagnosticsRequest -> do
          DaemonEnv {daemonOptions = opts} <- ask
          diagnostics <-
            if Options.daemonDryRun opts
              then do
                -- Return mock diagnostics in dry run without API calls
                pure $
                  Protocol.DaemonDiagnostics
                    { Protocol.diagCacheName = daemonCacheName,
                      Protocol.diagCacheUri = BinaryCache.uri daemonBinaryCache,
                      Protocol.diagCachePublic = BinaryCache.isPublic daemonBinaryCache,
                      Protocol.diagHasSigningKey = False,
                      Protocol.diagAuthOk = True,
                      Protocol.diagError = Just "[DRY RUN] Network requests disabled"
                    }
              else do
                -- Get push secret to check for signing key and auth token
                let pushParams = PushManager.pmPushParams daemonPushManager
                    pushSecret = pushParamsSecret pushParams
                    hasSigningKey = case pushSecret of
                      PushSigningKey {} -> True
                      PushToken {} -> False
                    maybeToken = getAuthTokenFromPushSecret pushSecret
                -- Verify auth by making an API call
                authResult <- case maybeToken of
                  Nothing -> pure $ Left "No auth token configured"
                  Just token -> do
                    res <- liftIO $ retryHttp $ (`runClientM` clientenv daemonEnv) $ API.getCache cachixClient token daemonCacheName
                    pure $ case res of
                      Right _ -> Right ()
                      Left err -> Left (show err)
                pure $
                  Protocol.DaemonDiagnostics
                    { Protocol.diagCacheName = daemonCacheName,
                      Protocol.diagCacheUri = BinaryCache.uri daemonBinaryCache,
                      Protocol.diagCachePublic = BinaryCache.isPublic daemonBinaryCache,
                      Protocol.diagHasSigningKey = hasSigningKey,
                      Protocol.diagAuthOk = isRight authResult,
                      Protocol.diagError = either Just (const Nothing) authResult
                    }
          SocketStore.sendAll socketId (Protocol.newMessage (Protocol.DaemonDiagnosticsResult diagnostics)) daemonClients
        _ -> return ()
    ShutdownGracefully -> do
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

installSignalHandlers :: Daemon ()
installSignalHandlers = do
  withRunInIO $ \runInIO -> do
    mainThreadId <- myThreadId
    -- Track Ctrl+C attempts
    interruptRef <- newIORef False

    -- Install signal handlers using runInIO to properly run Daemon actions from IO
    _ <- Signal.installHandler Signal.sigTERM (Signal.Catch (runInIO (termHandler mainThreadId))) Nothing
    _ <- Signal.installHandler Signal.sigINT (Signal.Catch (runInIO (intHandler mainThreadId interruptRef))) Nothing

    return ()
  where
    -- SIGTERM: Trigger immediate shutdown
    termHandler :: ThreadId -> Daemon ()
    termHandler mainThreadId = do
      Katip.logFM Katip.InfoS "sigTERM received. Exiting immediately..."
      startExitTimer mainThreadId
      liftIO CNix.Util.triggerInterrupt
      eventLoop <- asks daemonEventLoop
      -- Signal directly to the event loop to ensure exit even if queue is full
      EventLoop.exitLoopWithFailure EventLoopClosed eventLoop

    -- SIGINT: First try to shutdown gracefully, on second press force exit
    intHandler :: ThreadId -> IORef Bool -> Daemon ()
    intHandler mainThreadId interruptRef = do
      liftIO CNix.Util.triggerInterrupt
      isSecondInterrupt <- liftIO $ atomicModifyIORef' interruptRef (True,)
      eventLoop <- asks daemonEventLoop

      if isSecondInterrupt
        then do
          Katip.logFM Katip.InfoS "Exiting immediately..."
          startExitTimer mainThreadId
          -- Force shutdown at the event loop level to ensure exit even if queue is full
          EventLoop.exitLoopWithFailure EventLoopClosed eventLoop
        else do
          Katip.logFM Katip.InfoS "Shutting down gracefully (Ctrl+C again to force exit)..."
          EventLoop.send eventLoop ShutdownGracefully

    -- Start a timer to ensure we exit even if the event loop hangs
    startExitTimer :: ThreadId -> Daemon ()
    startExitTimer mainThreadId = do
      void $ liftIO $ forkIO $ do
        threadDelay (15 * 1000 * 1000) -- 15 seconds
        throwTo mainThreadId ExitSuccess

queueJob :: PushManager.PushJob -> Daemon Bool
queueJob pushJob = do
  DaemonEnv {daemonPushManager} <- ask
  liftIO $ PushManager.runPushManager daemonPushManager (PushManager.addPushJob pushJob)

subscribe :: DaemonEnv -> Maybe PushRequestId -> IO (TMChan PushEvent)
subscribe daemonEnv maybePushId = do
  chan <- liftIO newBroadcastTMChanIO
  liftIO $ atomically $ case maybePushId of
    Just pushId -> subscribeToSTM (daemonSubscriptionManager daemonEnv) pushId (SubChannel chan)
    Nothing -> subscribeToAllSTM (daemonSubscriptionManager daemonEnv) (SubChannel chan)
  liftIO $ atomically $ dupTMChan chan

subscribeAll :: DaemonEnv -> IO (TMChan PushEvent)
subscribeAll daemonEnv = subscribe daemonEnv Nothing

-- Publish messages from the channel to the client over the socket
publishToClient :: SocketId -> TMChan PushEvent -> DaemonEnv -> IO ()
publishToClient socketId chan daemonEnv = go
  where
    go = do
      msg <- atomically $ readTMChan chan
      case msg of
        Nothing -> return ()
        Just evt -> do
          let daemonMsg = DaemonPushEvent evt
          SocketStore.sendAll socketId (Protocol.newMessage daemonMsg) (daemonClients daemonEnv)
          go

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

  -- Stop the narinfo batch processor first (before closing the queue)
  liftIO $ PushManager.stopBatchProcessor daemonPushManager

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
      liftIO $ Listen.serverBye clientSock exitResult
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
