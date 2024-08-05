module Cachix.Client.Env
  ( Env (..),
    mkEnv,
    createClientEnv,
    customManagerSettings,
    Config.defaultCachixOptions,
  )
where

import Cachix.Client.Config (Config)
import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.OptionsParser as Options
import Cachix.Client.URI (getBaseUrl)
import Cachix.Client.Version (cachixVersion)
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
  { cachixoptions :: Config.CachixOptions,
    clientenv :: ClientEnv,
    config :: Config
  }

mkEnv :: Options.Flags -> IO Env
mkEnv flags = do
  -- make sure path to the config is passed as absolute to dhall logic
  canonicalConfigPath <- canonicalizePath (Options.configPath flags)
  cfg <- Config.getConfig canonicalConfigPath
  let cachixOptions =
        Config.CachixOptions
          { Config.configPath = canonicalConfigPath,
            Config.host = fromMaybe (Config.hostname cfg) (Options.hostname flags),
            Config.verbose = Options.verbose flags
          }
  clientEnv <- createClientEnv cachixOptions
  return
    Env
      { cachixoptions = cachixOptions,
        clientenv = clientEnv,
        config = cfg
      }

customManagerSettings :: ManagerSettings
customManagerSettings =
  tlsManagerSettings
    { managerResponseTimeout = responseTimeoutNone,
      -- managerModifyRequest :: Request -> IO Request
      managerModifyRequest = return . setRequestHeader "User-Agent" [toS cachixVersion]
    }

createClientEnv :: Config.CachixOptions -> IO ClientEnv
createClientEnv cachixOptions = do
  manager <- newTlsManagerWith customManagerSettings
  return $ mkClientEnv manager $ getBaseUrl (Config.host cachixOptions)
