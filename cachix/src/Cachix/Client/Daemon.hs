{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.Daemon
  ( run,
    runWithSocket,
    push,
    stop,
  )
where

import Cachix.Client.CNix (filterInvalidStorePath)
import qualified Cachix.Client.Commands as Commands
import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Client (push, stop)
import Cachix.Client.Daemon.Listen as Daemon
import Cachix.Client.Daemon.Types
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser as Options
import Cachix.Client.Push
import Control.Concurrent.Extra (once)
import Control.Concurrent.STM.TBMQueue
import qualified Control.Immortal as Immortal
import Data.String.Here (i)
import qualified Data.Text as T
import qualified Hercules.CNix.Store as Store
import qualified Network.Socket as Socket
import Protolude
import System.Posix.Process (getProcessID)

run :: Env -> DaemonOptions -> PushOptions -> IO ()
run env daemonOptions pushOptions = do
  socketPath <- maybe getSocketPath pure (daemonSocketPath daemonOptions)
  runWithSocket env pushOptions socketPath
    `finally` putErrText "Daemon shut down."

runWithSocket :: Env -> PushOptions -> FilePath -> IO ()
runWithSocket env pushOptions socketPath = do
  -- Create a queue of push requests for the workers to process
  queue <- newTBMQueueIO 1000

  bracket (startWorker queue) identity $ \shutdownWorker -> do
    -- TODO: retry the connection on socket errors
    bracket (Daemon.openSocket socketPath) Socket.close $ \sock -> do
      Socket.listen sock Socket.maxListenQueue

      putText =<< readyMessage socketPath
      clientStopConn <- Daemon.listen queue sock

      -- Gracefully shutdown the worker before closing the socket
      -- TODO: consider shutdown from Network.Socket
      shutdownWorker

      Socket.gracefulClose clientStopConn 5000
  where
    startWorker queue = do
      worker <- Immortal.create $ \thread ->
        Immortal.onUnexpectedFinish thread logWorkerException $
          runWorker env pushOptions queue

      once $ do
        putErrText "Shutting down daemon..."
        atomically $ closeTBMQueue queue
        Immortal.mortalize worker
        putErrText "Waiting for worker to finish..."
        Immortal.wait worker

logWorkerException :: Either SomeException () -> IO ()
logWorkerException (Left err) =
  putErrText $ "Exception in daemon worker thread: " <> show err
logWorkerException _ = return ()

runWorker :: Env -> PushOptions -> TBMQueue QueuedPushRequest -> IO ()
runWorker env pushOptions queue =
  atomically (readTBMQueue queue) >>= \case
    Nothing -> return ()
    Just msg -> handleRequest env pushOptions msg

handleRequest :: Env -> PushOptions -> QueuedPushRequest -> IO ()
handleRequest env pushOptions (QueuedPushRequest {..}) = do
  Commands.withPushParams env pushOptions (binaryCacheName pushRequest) $ \pushParams -> do
    normalized <-
      for (storePaths pushRequest) $ \fp -> do
        storePath <- Store.followLinksToStorePath (pushParamsStore pushParams) (encodeUtf8 $ T.pack fp)
        filterInvalidStorePath (pushParamsStore pushParams) storePath

    void $
      pushClosure
        (mapConcurrentlyBounded (numJobs pushOptions))
        pushParams
        (catMaybes normalized)

readyMessage :: FilePath -> IO Text
readyMessage socketPath = do
  -- Get the PID of the process
  pid <- getProcessID
  return
    [i|
Cachix Daemon is ready to push store paths.
PID: ${show pid :: Text}
Listening on socket: ${socketPath}
  |]
