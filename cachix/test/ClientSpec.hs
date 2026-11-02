module ClientSpec (spec) where

import Protolude
import StalledServer (withStalledServer)
import System.Environment (getEnvironment)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Process qualified as Process
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "SIGTERM during stalled requests" $
  forM_ ["http", "https"] $ \scheme -> do
    it (toS $ "stops cachix use over " <> scheme) $
      terminatesOnSignal scheme ["use", "somecache"]
    it (toS $ "stops daemon startup over " <> scheme) $
      terminatesOnSignal scheme ["daemon", "run", "somecache"]

terminatesOnSignal :: Text -> [Text] -> IO ()
terminatesOnSignal scheme command =
  withSystemTempDirectory "cachix-signal-test" $ \dir -> withFile (dir <> "/client.log") WriteMode $ \output ->
    withStalledServer $ \port connected -> do
      environment <- getEnvironment
      let overrides =
            [ ("CACHIX_AUTH_TOKEN", "test-token"),
              ("no_proxy", "127.0.0.1"),
              ("NO_PROXY", "127.0.0.1"),
              ("CACHIX_DAEMON_SOCKET", dir <> "/daemon.sock"),
              ("XDG_CONFIG_HOME", dir)
            ]
          args = ["--config", dir <> "/cachix.dhall", "--hostname", toS scheme <> "://127.0.0.1:" <> show port] <> map toS command
          process =
            (Process.proc "cachix" args)
              { Process.env = Just (overrides <> filter (\(key, _) -> key `notElem` ("CACHIX_SIGNING_KEY" : map fst overrides)) environment),
                Process.std_out = Process.UseHandle output,
                Process.std_err = Process.UseHandle output
              }
      bracket (Process.createProcess process) cleanup $ \(_, _, _, processHandle) -> do
        -- Wait for network I/O so the signal tests the initialized client,
        -- rather than racing its startup and signal-handler installation.
        timeout (10 * 1_000_000) connected `shouldReturn` Just ()
        pid <- Process.getPid processHandle >>= maybe (panic "Missing client PID") pure
        signalProcess sigTERM pid
        result <- timeout (5 * 1_000_000) (Process.waitForProcess processHandle)
        result `shouldSatisfy` maybe False (/= ExitSuccess)
  where
    cleanup (_, _, _, processHandle) = do
      Process.getProcessExitCode processHandle >>= \case
        Just _ -> pure ()
        Nothing -> do
          pid <- Process.getPid processHandle
          for_ pid $ signalProcess sigKILL
          void $ Process.waitForProcess processHandle
