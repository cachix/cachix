module Cachix.Client.Env
  ( Env(..)
  , mkEnv
  , cachixVersion
  ) where

import Protolude

import Data.Version                (showVersion)
import Paths_cachix                (version)

import Network.HTTP.Simple         ( setRequestHeader )
import Network.HTTP.Client.TLS     ( newTlsManagerWith, tlsManagerSettings )
import Network.HTTP.Client         ( managerResponseTimeout, managerModifyRequest
                                   , responseTimeoutNone, ManagerSettings)
import Servant.Client              ( mkClientEnv, ClientEnv )

import Cachix.Client.Config        ( readConfig, Config )
import Cachix.Client.OptionsParser ( CachixOptions(..) )
import Cachix.Client.URI           ( getBaseUrl )


data Env = Env
  { config :: Maybe Config
  , clientenv :: ClientEnv
  , cachixoptions :: CachixOptions
  }


mkEnv :: CachixOptions -> IO Env
mkEnv cachixoptions = do
  config <- readConfig $ configPath cachixoptions
  manager <- newTlsManagerWith customManagerSettings
  let clientenv = mkClientEnv manager $ getBaseUrl (host cachixoptions)
  return $ Env
    { config = config
    , clientenv = clientenv
    , cachixoptions = cachixoptions
    }

customManagerSettings :: ManagerSettings
customManagerSettings = tlsManagerSettings
  { managerResponseTimeout = responseTimeoutNone
  -- managerModifyRequest :: Request -> IO Request
  , managerModifyRequest = return . setRequestHeader "User-Agent" [toS cachixVersion]
  }

cachixVersion :: Text
cachixVersion = "cachix " <> toS (showVersion version)
