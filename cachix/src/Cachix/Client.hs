module Cachix.Client
  ( main,
  )
where

import Cachix.Client.Command qualified as Command
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (cachixoptions, mkEnv)
import Cachix.Client.Exception (CachixException (DeprecatedCommand))
import Cachix.Client.OptionsParser
  ( CachixCommand (..),
    DaemonCommand (..),
    PushArguments (..),
    daemonNarinfoQueryOptions,
    getOpts,
  )
import Cachix.Client.Version (cachixVersion)
import Cachix.Daemon qualified as Daemon
import Cachix.Daemon.Client qualified as Daemon.Client
import Cachix.Deploy.ActivateCommand as ActivateCommand
import Cachix.Deploy.Agent qualified as AgentCommand
import Cachix.Deploy.OptionsParser qualified as DeployOptions
import Hercules.CNix qualified as CNix
import Protolude
import System.Console.AsciiProgress (displayConsoleRegions)
import System.Posix.Signals qualified as Signal

main :: IO ()
main = displayConsoleRegions $ do
  (flags, command) <- getOpts
  env <- mkEnv flags

  initNixStore
  installSignalHandlers

  let cachixOptions = cachixoptions env
  case command of
    AuthToken token -> Command.authtoken env token
    Config configCommand -> Config.run cachixOptions configCommand
    Daemon (DaemonRun daemonOptions pushOptions mcacheName) -> Daemon.start env daemonOptions pushOptions mcacheName
    Daemon (DaemonStop daemonOptions) -> Daemon.Client.stop env daemonOptions
    Daemon (DaemonPushPaths daemonOptions daemonPushOptions storePaths) -> Daemon.Client.push env daemonOptions daemonPushOptions storePaths
    Daemon (DaemonWatchExec daemonOptions pushOptions cacheName cmd args) -> Command.watchExecDaemon env pushOptions (daemonNarinfoQueryOptions daemonOptions) cacheName cmd args
    DeployCommand (DeployOptions.Agent opts) -> AgentCommand.run cachixOptions opts
    DeployCommand (DeployOptions.Activate opts) -> ActivateCommand.run env opts
    Doctor doctorOptions -> Command.doctor env doctorOptions
    GenerateKeypair name -> Command.generateKeypair env name
    Import pushOptions name uri -> Command.import' env pushOptions name uri
    Pin pingArgs -> Command.pin env pingArgs
    Push (PushPaths opts name cliPaths) -> Command.push env opts name cliPaths
    Push (PushWatchStore _ _) ->
      throwIO $ DeprecatedCommand "DEPRECATED: cachix watch-store has replaced cachix push --watch-store."
    Remove name -> Command.remove env name
    Use name useOptions -> Command.use env name useOptions
    Version -> putText cachixVersion
    WatchExec watchExecMode pushArgs batchOptions name cmd args ->
      Command.watchExec env watchExecMode pushArgs batchOptions name cmd args
    WatchStore watchArgs name -> Command.watchStore env watchArgs name

-- | Install client-wide signal handlers.
installSignalHandlers :: IO ()
installSignalHandlers = do
  -- Ignore sigPIPE.
  -- The default handler terminates the process.
  -- WARN: may be reset to SIG_DFL when initializing the Nix library.
  _ <- Signal.installHandler Signal.sigPIPE Signal.Ignore Nothing

  return ()

-- | Initialize the Nix library via CNix.
initNixStore :: IO ()
initNixStore = do
  signalset <- Signal.getSignalMask

  -- Initialize the Nix library
  CNix.init

  -- darwin: restore the signal mask modified by Nix
  -- https://github.com/cachix/cachix/issues/501
  Signal.setSignalMask signalset
