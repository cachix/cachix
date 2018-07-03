module Cachix.Client
  ( main
  ) where

import Data.Version                (showVersion)
import Paths_cachix                (version)
import Protolude
import Network.HTTP.Simple         ( setRequestHeader )
import Network.HTTP.Client.TLS     ( newTlsManagerWith, tlsManagerSettings )
import Network.HTTP.Client         ( managerResponseTimeout, managerModifyRequest
                                   , responseTimeoutNone, ManagerSettings)
import Servant.Client              ( mkClientEnv)

import Cachix.Client.OptionsParser ( CachixCommand(..), CachixOptions(..), getOpts )
import Cachix.Client.Config        ( readConfig )
import Cachix.Client.Commands      as Commands
import Cachix.Client.URI           ( getBaseUrl)


main :: IO ()
main = do
  (CachixOptions{..}, command) <- getOpts
  config <- readConfig
  manager <- newTlsManagerWith customManagerSettings
  let env = mkClientEnv manager $ getBaseUrl host
  case command of -- TODO: might want readerT here with client, config and env and opts
    AuthToken token -> Commands.authtoken env config token
    Create name -> Commands.create env config name
    Push name paths watchStore -> Commands.push env config name paths watchStore
    Use name shouldEchoNixOS -> Commands.use env config name shouldEchoNixOS
    Version -> putText cachixVersion

customManagerSettings :: ManagerSettings
customManagerSettings = tlsManagerSettings
  { managerResponseTimeout = responseTimeoutNone
  -- managerModifyRequest :: Request -> IO Request
  , managerModifyRequest = return . setRequestHeader "User-Agent" [toS cachixVersion]
  }

cachixVersion :: Text
cachixVersion = "cachix " <> toS (showVersion version)
