{-# LANGUAGE QuasiQuotes #-}
module Cachix.Client
  ( main
  ) where

import Protolude
import Network.HTTP.Client.TLS     (newTlsManager)
import URI.ByteString.QQ
import Servant.Client              (mkClientEnv)

import Cachix.Client.OptionsParser (CachixCommand(..), getOpts)
import Cachix.Client.Config        (readConfig)
import Cachix.Client.Commands      as Commands
import Cachix.Client.URI           (getBaseUrl)


main :: IO ()
main = do
  opts <- getOpts
  config <- readConfig
  manager <- newTlsManager
  let env = mkClientEnv manager $ getBaseUrl [uri|http://localhost:8090|] -- TODO: global cli arg
  case opts of -- TODO: we might want readerT here with client, config and env
    AuthToken token -> Commands.authtoken env config token
    Create name -> Commands.create env config name
    Sync maybeName -> Commands.sync env config maybeName
    Use name -> Commands.use env config name
