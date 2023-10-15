module Cachix.Client
  ( main,
  )
where

import Cachix.Client.Commands as Commands
import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.Daemon as Daemon
import Cachix.Client.Env (cachixoptions, mkEnv)
import Cachix.Client.OptionsParser (CachixCommand (..), DaemonCommand (..), getOpts)
import Cachix.Client.Version (cachixVersion)
import Cachix.Deploy.ActivateCommand as ActivateCommand
import qualified Cachix.Deploy.Agent as AgentCommand
import qualified Cachix.Deploy.OptionsParser as DeployOptions
import Protolude
import System.Console.AsciiProgress (displayConsoleRegions)

main :: IO ()
main = displayConsoleRegions $ do
  (flags, command) <- getOpts
  env <- mkEnv flags
  let cachixOptions = cachixoptions env
  case command of
    AuthToken token -> Commands.authtoken env token
    Config configCommand -> Config.run cachixOptions configCommand
    Daemon (DaemonRun daemonOptions pushOptions mcacheName) -> Daemon.start env daemonOptions pushOptions mcacheName
    Daemon (DaemonStop daemonOptions) -> Daemon.stopAndWait env daemonOptions
    Daemon (DaemonPushPaths daemonOptions storePaths) -> Daemon.push env daemonOptions storePaths
    Daemon (DaemonWatchExec pushOptions cacheName cmd args) -> Commands.watchExecDaemon env pushOptions cacheName cmd args
    GenerateKeypair name -> Commands.generateKeypair env name
    Push pushArgs -> Commands.push env pushArgs
    Pin pingArgs -> Commands.pin env pingArgs
    WatchStore watchArgs name -> Commands.watchStore env watchArgs name
    WatchExec pushArgs name cmd args -> Commands.watchExec env pushArgs name cmd args
    Use name useOptions -> Commands.use env name useOptions
    Remove name -> Commands.remove env name
    Version -> putText cachixVersion
    DeployCommand (DeployOptions.Agent opts) -> AgentCommand.run cachixOptions opts
    DeployCommand (DeployOptions.Activate opts) -> ActivateCommand.run env opts
