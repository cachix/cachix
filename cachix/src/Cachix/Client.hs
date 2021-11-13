module Cachix.Client
  ( main,
  )
where

import Cachix.Client.Commands as Commands
import Cachix.Client.Env (mkEnv)
import Cachix.Client.OptionsParser (CachixCommand (..), getOpts)
import Cachix.Client.Version (cachixVersion)
import Cachix.Deploy.ActivateCommand as ActivateCommand
import qualified Cachix.Deploy.Agent as AgentCommand
import qualified Cachix.Deploy.OptionsParser as DeployOptions
import Protolude

main :: IO ()
main = do
  (cachixoptions, command) <- getOpts
  env <- mkEnv cachixoptions
  case command of
    AuthToken token -> Commands.authtoken env token
    GenerateKeypair name -> Commands.generateKeypair env name
    Push pushArgs -> Commands.push env pushArgs
    WatchStore watchArgs name -> Commands.watchStore env watchArgs name
    WatchExec pushArgs name cmd args -> Commands.watchExec env pushArgs name cmd args
    Use name useOptions -> Commands.use env name useOptions
    Version -> putText cachixVersion
    DeployCommand deployCommand ->
      case deployCommand of
        DeployOptions.Agent opts -> AgentCommand.run cachixoptions opts
        DeployOptions.Activate opts -> ActivateCommand.run cachixoptions opts
