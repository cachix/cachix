module Cachix.Client
  ( main,
  )
where

import Cachix.Client.Commands as Commands
import qualified Cachix.Client.Config as Config
import Cachix.Client.Env (cachixoptions, mkEnv)
import Cachix.Client.OptionsParser (CachixCommand (..), getOpts)
import Cachix.Client.Version (cachixVersion)
import Cachix.Deploy.ActivateCommand as ActivateCommand
import qualified Cachix.Deploy.Agent as AgentCommand
import qualified Cachix.Deploy.OptionsParser as DeployOptions
import Protolude

main :: IO ()
main = do
  (flags, command) <- getOpts
  env <- mkEnv flags
  let cachixOptions = cachixoptions env
  case command of
    AuthToken token -> Commands.authtoken env token
    Config configCommand -> Config.run cachixOptions configCommand
    GenerateKeypair name -> Commands.generateKeypair env name
    Push pushArgs -> Commands.push env pushArgs
    Pin pingArgs -> Commands.pin env pingArgs
    WatchStore watchArgs name -> Commands.watchStore env watchArgs name
    WatchExec pushArgs name cmd args -> Commands.watchExec env pushArgs name cmd args
    Use name useOptions -> Commands.use env name useOptions
    Version -> putText cachixVersion
    DeployCommand deployCommand ->
      case deployCommand of
        DeployOptions.Agent opts -> AgentCommand.run cachixOptions opts
        DeployOptions.Activate opts -> ActivateCommand.run env opts
