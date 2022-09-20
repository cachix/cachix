module Cachix.Client.Env
  ( Env (..),
    mkEnv,
    createClientEnv,
    customManagerSettings,
  )
where

import Cachix.Client.Config (CachixOptions (..), Config, getConfig)
import Cachix.Client.URI (getBaseUrl)
import Cachix.Client.Version (cachixVersion)
import Hercules.CNix.Store (Store, openStore)
import Network.HTTP.Client
  ( ManagerSettings,
    managerModifyRequest,
    managerResponseTimeout,
    responseTimeoutNone,
  )
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Simple (setRequestHeader)
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Client.Streaming (ClientEnv, mkClientEnv)
import System.Directory (canonicalizePath)

data Env = Env
  { config :: Config,
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
  cfg <- getConfig (configPath cachixOptions)
  clientEnv <- createClientEnv cachixOptions
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

createClientEnv :: CachixOptions -> IO ClientEnv
createClientEnv cachixOptions = do
  manager <- newTlsManagerWith customManagerSettings
  return $ mkClientEnv manager $ getBaseUrl (host cachixOptions)
