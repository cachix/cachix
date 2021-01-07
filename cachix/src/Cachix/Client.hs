module Cachix.Client
  ( main,
  )
where

import Cachix.Client.Commands as Commands
import Cachix.Client.Env (cachixVersion, mkEnv)
import Cachix.Client.OptionsParser (CachixCommand (..), getOpts)
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
    Use name useOptions -> Commands.use env name useOptions
    Version -> putText cachixVersion
