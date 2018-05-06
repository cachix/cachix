module Cachix.Client
  ( main
  ) where

import Protolude
import Network.HTTP.Client.TLS     (newTlsManagerWith)
import Network.HTTP.Client         (defaultManagerSettings, managerResponseTimeout
                                   , responseTimeoutNone, ManagerSettings)

import Servant.Client              (mkClientEnv)

import Cachix.Client.OptionsParser (CachixCommand(..), CachixOptions(..), getOpts)
import Cachix.Client.Config        (readConfig)
import Cachix.Client.Commands      as Commands
import Cachix.Client.URI           (getBaseUrl)


main :: IO ()
main = do
  (CachixOptions{..}, command) <- getOpts
  config <- readConfig
  manager <- newTlsManagerWith customManagerSettings
  let env = mkClientEnv manager $ getBaseUrl host
  case command of -- TODO: might want readerT here with client, config and env and opts
    AuthToken token -> Commands.authtoken env config token
    Create name -> Commands.create env config name
    Sync name -> Commands.sync env config name
    Use name -> Commands.use env config name

customManagerSettings :: ManagerSettings
customManagerSettings = defaultManagerSettings
  { managerResponseTimeout = responseTimeoutNone
  }
