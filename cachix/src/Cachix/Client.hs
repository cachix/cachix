module Cachix.Client
  ( main
  ) where

import Protolude

import Cachix.Client.OptionsParser ( CachixCommand(..), CachixOptions(..), getOpts )
import Cachix.Client.Commands      as Commands
import Cachix.Client.Env           ( mkEnv, cachixVersion )


main :: IO ()
main = do
  (cachixoptions, command) <- getOpts
  env <- mkEnv cachixoptions
  case command of
    AuthToken token -> Commands.authtoken env token
    Create name -> Commands.create env name
    Push name paths watchStore -> Commands.push env name paths watchStore
    Use name shouldEchoNixOS -> Commands.use env name shouldEchoNixOS
    Version -> putText cachixVersion
