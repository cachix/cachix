module Cachix.Client
  ( main,
  )
where

import Cachix.Client.Commands as Commands
import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.Daemon as Daemon
import qualified Cachix.Client.Daemon.Client as Daemon.Client
import Cachix.Client.Env (cachixoptions, mkEnv)
import Cachix.Client.OptionsParser (CachixCommand (..), DaemonCommand (..), getOpts)
import Cachix.Client.Version (cachixVersion)
import Cachix.Deploy.ActivateCommand as ActivateCommand
import qualified Cachix.Deploy.Agent as AgentCommand
import qualified Cachix.Deploy.OptionsParser as DeployOptions
import qualified Hercules.CNix as CNix
import qualified Hercules.CNix.Util as CNix.Util
import Protolude
import System.Console.AsciiProgress (displayConsoleRegions)
import qualified System.Posix.Signals as Signal

main :: IO ()
main = displayConsoleRegions $ do
  (flags, command) <- getOpts
  env <- mkEnv flags

  initNixStore

  installSignalHandlers

  let cachixOptions = cachixoptions env
  case command of
    AuthToken token -> Commands.authtoken env token
    Config configCommand -> Config.run cachixOptions configCommand
    Daemon (DaemonRun daemonOptions pushOptions mcacheName) -> Daemon.start env daemonOptions pushOptions mcacheName
    Daemon (DaemonStop daemonOptions) -> Daemon.Client.stop env daemonOptions
    Daemon (DaemonPushPaths daemonOptions storePaths) -> Daemon.Client.push env daemonOptions storePaths
    Daemon (DaemonWatchExec pushOptions cacheName cmd args) -> Commands.watchExecDaemon env pushOptions cacheName cmd args
    GenerateKeypair name -> Commands.generateKeypair env name
    Push pushArgs -> Commands.push env pushArgs
    Pin pingArgs -> Commands.pin env pingArgs
    Import pushOptions name uri -> Commands.import' env pushOptions name uri
    WatchStore watchArgs name -> Commands.watchStore env watchArgs name
    WatchExec pushArgs name cmd args -> Commands.watchExec env pushArgs name cmd args
    Use name useOptions -> Commands.use env name useOptions
    Remove name -> Commands.remove env name
    Version -> putText cachixVersion
    DeployCommand (DeployOptions.Agent opts) -> AgentCommand.run cachixOptions opts
    DeployCommand (DeployOptions.Activate opts) -> ActivateCommand.run env opts

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

  -- Interrupt Nix before throwing UserInterrupt
  CNix.Util.installDefaultSigINTHandler
