module Cachix.Client.Command.Watch
  ( watchExec,
    watchStore,
    watchExecDaemon,
    watchExecStore,
  )
where

import Cachix.Client.Command.Push
import Cachix.Client.Env (Env (..))
import Cachix.Client.InstallationMode qualified as InstallationMode
import Cachix.Client.OptionsParser
  ( DaemonOptions (..),
    PushOptions (..),
    WatchExecMode (..),
  )
import Cachix.Client.Push
import Cachix.Client.WatchStore qualified as WatchStore
import Cachix.Daemon qualified as Daemon
import Cachix.Daemon.NarinfoQuery (NarinfoQueryOptions)
import Cachix.Daemon.PostBuildHook qualified as Daemon.PostBuildHook
import Cachix.Daemon.Progress qualified as Daemon.Progress
import Cachix.Daemon.Types
import Cachix.Types.BinaryCache (BinaryCacheName)
import Conduit
import Control.Concurrent.Async qualified as Async
import Data.Conduit.Combinators qualified as C
import Data.Conduit.TMChan qualified as C
import Data.Generics.Labels ()
import Data.Text.IO (hGetLine)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Hercules.CNix.Store (withStore)
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Conduit ()
import System.Console.Pretty
import System.Environment (getEnvironment)
import System.IO.Error (isEOFError)
import System.IO.Temp (withTempFile)
import System.Posix.Signals qualified as Signals
import System.Process qualified

watchStore :: Env -> PushOptions -> Text -> IO ()
watchStore env opts name = do
  withPushParams env opts name $ \pushParams ->
    WatchStore.startWorkers (pushParamsStore pushParams) (numJobs opts) pushParams

-- | Run a command and upload any new paths to the binary cache.
--
-- In auto mode, registers a post-build hook if the user is trusted.
-- Otherwise, falls back to watching the entire Nix store.
watchExec :: Env -> WatchExecMode -> PushOptions -> NarinfoQueryOptions -> BinaryCacheName -> Text -> [Text] -> IO ()
watchExec env watchExecMode pushOptions batchOptions cacheName cmd args = do
  case watchExecMode of
    PostBuildHook ->
      watchExecDaemon env pushOptions batchOptions cacheName cmd args
    Store ->
      watchExecStore env pushOptions cacheName cmd args
    Auto -> do
      nixEnv <- InstallationMode.getNixEnv

      if InstallationMode.isTrusted nixEnv
        then watchExecDaemon env pushOptions batchOptions cacheName cmd args
        else do
          putErrText fallbackWarning
          watchExecStore env pushOptions cacheName cmd args
  where
    fallbackWarning =
      color Yellow "WARNING: " <> "failed to register a post-build hook for this command because you're not a trusted user. Falling back to watching the entire Nix store for new paths."

-- | Run a command and push any new paths to the binary cache.
--
-- Requires the user to be a trusted user in a multi-user installation.
watchExecDaemon :: Env -> PushOptions -> NarinfoQueryOptions -> BinaryCacheName -> Text -> [Text] -> IO ()
watchExecDaemon env pushOpts batchOptions cacheName cmd args =
  Daemon.PostBuildHook.withSetup Nothing $ \hookEnv ->
    withTempFile (Daemon.PostBuildHook.tempDir hookEnv) "daemon-log-capture" $ \_ logHandle ->
      withStore $ \store -> do
        let daemonOptions =
              DaemonOptions
                { daemonAllowRemoteStop = False,
                  daemonDryRun = False,
                  daemonNarinfoQueryOptions = batchOptions,
                  daemonSocketPath = Just (Daemon.PostBuildHook.daemonSock hookEnv)
                }
        daemon <- Daemon.new env store daemonOptions (Just logHandle) pushOpts cacheName

        exitCode <-
          bracket (startDaemonThread daemon) (shutdownDaemonThread daemon logHandle) $ \_ -> do
            processEnv <- getEnvironment
            let newProcessEnv = Daemon.PostBuildHook.modifyEnv (Daemon.PostBuildHook.envVar hookEnv) processEnv
            let process =
                  (System.Process.proc (toS cmd) (toS <$> args))
                    { System.Process.std_out = System.Process.Inherit,
                      System.Process.env = Just newProcessEnv,
                      System.Process.delegate_ctlc = True
                    }
            System.Process.withCreateProcess process $ \_ _ _ processHandle ->
              System.Process.waitForProcess processHandle

        exitWith exitCode
  where
    -- Launch the daemon in the background and subscribe to all push events
    startDaemonThread daemon = do
      daemonThread <- Async.async $ Daemon.run daemon
      daemonChan <- Daemon.subscribeAll daemon
      return (daemonThread, daemonChan)

    shutdownDaemonThread daemon logHandle (daemonThread, daemonChan) = do
      -- TODO: process and fold events into a state during command execution
      daemonRes <- Async.withAsync (postWatchExec daemonChan) $ \_ -> do
        Daemon.stopIO daemon
        Async.wait daemonThread

      -- Print the daemon log in case there was an internal error
      case toExitCode daemonRes of
        ExitFailure _ -> printLog logHandle
        ExitSuccess -> return ()

    postWatchExec chan = do
      progressState <- Daemon.Progress.newProgressState
      runConduit $
        C.sourceTMChan chan
          .| C.mapM_ (liftIO . Daemon.Progress.displayPushEvent progressState)

    printLog h = getLineLoop
      where
        getLineLoop = do
          eline <- try $ hGetLine h
          case eline of
            Left e
              | isEOFError e -> return ()
              | otherwise -> putErrText $ "Error reading daemon log: " <> show e
            Right line -> do
              hPutStr stderr line
              getLineLoop

-- | Runs a command while watching the entire Nix store and pushing any new paths.
--
-- Prefer to use the more granular 'watchExecDaemon' whenever possible.
watchExecStore :: Env -> PushOptions -> BinaryCacheName -> Text -> [Text] -> IO ()
watchExecStore env pushOpts name cmd args =
  withPushParams env pushOpts name $ \pushParams -> do
    stdoutOriginal <- hDuplicate stdout
    let process =
          (System.Process.proc (toS cmd) (toS <$> args))
            { System.Process.std_out = System.Process.UseHandle stdoutOriginal
            }
        watch = do
          hDuplicateTo stderr stdout -- redirect all stdout to stderr
          WatchStore.startWorkers (pushParamsStore pushParams) (numJobs pushOpts) pushParams

    (_, exitCode) <-
      Async.concurrently watch $ do
        exitCode <-
          bracketOnError
            (getProcessHandle <$> System.Process.createProcess process)
            ( \processHandle -> do
                -- Terminate the process
                uninterruptibleMask_ (System.Process.terminateProcess processHandle)
                -- Wait for the process to clean up and exit
                _ <- System.Process.waitForProcess processHandle
                -- Stop watching the store and wait for all paths to be pushed
                Signals.raiseSignal Signals.sigINT
            )
            System.Process.waitForProcess

        -- Stop watching the store and wait for all paths to be pushed
        Signals.raiseSignal Signals.sigINT
        return exitCode

    exitWith exitCode
  where
    getProcessHandle (_, _, _, processHandle) = processHandle
