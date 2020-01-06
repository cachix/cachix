module Cachix.Client.Env
  ( Env (..),
    mkEnv,
    cachixVersion,
  )
where

import Cachix.Client.Config (Config, readConfig)
import Cachix.Client.OptionsParser (CachixOptions (..))
import Cachix.Client.Store (Store, openStore)
import Cachix.Client.URI (getBaseUrl)
import Control.Concurrent.Async (Async, async)
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
  let cachixoptions = rawcachixoptions {configPath = canonicalConfigPath}
  config <- readConfig $ configPath cachixoptions
  manager <- newTlsManagerWith customManagerSettings
  let clientenv = mkClientEnv manager $ getBaseUrl (host cachixoptions)
  return Env
    { config = config,
      clientenv = clientenv,
      cachixoptions = cachixoptions,
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
