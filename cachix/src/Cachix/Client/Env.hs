module Cachix.Client.Env
  ( Env (..),
    mkEnv,
    cachixVersion,
    customManagerSettings,
  )
where

import Cachix.Client.Config (Config, readConfig)
import Cachix.Client.OptionsParser (CachixOptions (..))
import Cachix.Client.Store (Store, openStore)
import Cachix.Client.URI (getBaseUrl)
import Data.Version (showVersion)
import Network.HTTP.Client
  ( ManagerSettings,
    managerModifyRequest,
    managerResponseTimeout,
    responseTimeoutNone,
  )
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Simple (setRequestHeader)
import Paths_cachix (version)
import Protolude
import Servant.Client (ClientEnv, mkClientEnv)
import System.Directory (canonicalizePath)

data Env
  = Env
      { config :: Maybe Config,
        clientenv :: ClientEnv,
        cachixoptions :: CachixOptions,
        storeAsync :: Async Store
      }

mkEnv :: CachixOptions -> IO Env
mkEnv rawcachixoptions = do
  store <- async openStore
  -- make sure path to the config is passed as absolute to dhall logic
  canonicalConfigPath <- canonicalizePath (configPath rawcachixoptions)
  let cachixOptions = rawcachixoptions {configPath = canonicalConfigPath}
  cfg <- readConfig $ configPath cachixOptions
  manager <- newTlsManagerWith customManagerSettings
  let clientEnv = mkClientEnv manager $ getBaseUrl (host cachixOptions)
  return
    Env
      { config = cfg,
        clientenv = clientEnv,
        cachixoptions = cachixOptions,
        storeAsync = store
      }

customManagerSettings :: ManagerSettings
customManagerSettings =
  tlsManagerSettings
    { managerResponseTimeout = responseTimeoutNone,
      -- managerModifyRequest :: Request -> IO Request
      managerModifyRequest = return . setRequestHeader "User-Agent" [toS cachixVersion]
    }

cachixVersion :: Text
cachixVersion = "cachix " <> toS (showVersion version)
