{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.Daemon
  ( run,
    runWithSocket,
    push,
  )
where

import Cachix.Client.CNix (filterInvalidStorePath)
import qualified Cachix.Client.Commands as Commands
import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Listen as Daemon
import Cachix.Client.Daemon.Push (push)
import Cachix.Client.Daemon.Types
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser as Options
import Cachix.Client.Push
import Control.Concurrent.STM.TBMQueue
import qualified Control.Immortal as Immortal
import Data.String.Here (i)
import qualified Data.Text as T
import qualified Hercules.CNix.Store as Store
import qualified Network.Socket as Socket
import Protolude
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)

run :: Env -> PushOptions -> DaemonOptions -> IO ()
run env pushOptions daemonOptions = do
  socketPath <- maybe getSocketPath pure (daemonSocketPath daemonOptions)
  runWithSocket env pushOptions socketPath

runWithSocket :: Env -> PushOptions -> FilePath -> IO ()
runWithSocket env pushOptions socketPath =
  bracket (open socketPath) Socket.close $ \sock -> do
    Socket.listen sock Socket.maxListenQueue

    -- Create a queue of push requests for the workers to process
    queue <- newTBMQueueIO 1000
    bracket (startWorkers queue) (shutdownWorkers queue) $ \_ -> do
      putText $ readyMessage socketPath
      Daemon.listen queue sock
  where
    startWorkers queue =
      replicateM 1 $ do
        Immortal.create $ \thread ->
          Immortal.onUnexpectedFinish thread logException $
            runWorker env pushOptions queue

    shutdownWorkers queue workers = do
      atomically $ closeTBMQueue queue
      mapM_ Immortal.mortalize workers
      mapM_ Immortal.wait workers

    logException e =
      putErrText $ "Exception in daemon worker thread: " <> show e

    -- TODO: lock the socket
    open socketFilePath = do
      deleteSocketFileIfExists socketFilePath
      sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
      Socket.bind sock $ Socket.SockAddrUnix socketFilePath
      -- setFileMode socketFilePath socketFileMode
      return sock

    deleteSocketFileIfExists path =
      removeFile path `catch` handleDoesNotExist

    handleDoesNotExist e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

runWorker :: Env -> PushOptions -> TBMQueue QueuedPushRequest -> IO ()
runWorker env pushOptions queue =
  atomically (readTBMQueue queue) >>= \case
    Nothing -> return ()
    Just msg -> handleRequest env pushOptions msg

handleRequest :: Env -> PushOptions -> QueuedPushRequest -> IO ()
handleRequest env pushOptions (QueuedPushRequest {..}) = do
  putText $ "Pushing " <> show (length $ storePaths pushRequest) <> " store paths"

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

readyMessage :: FilePath -> Text
readyMessage socketPath =
  [i|
Cachix Daemon is ready to push store paths.
Listening on socket: ${socketPath}
  |]
