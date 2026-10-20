module Cachix.Client.Env
  ( Env (..),
    mkEnv,
    effectiveHost,
    createClientEnv,
    customManagerSettings,
    Config.defaultCachixOptions,
  )
where

import Cachix.Client.Config (Config)
import Cachix.Client.Config qualified as Config
import Cachix.Client.OptionsParser qualified as Options
import Cachix.Client.URI (URI, getBaseUrl)
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

-- | The Cachix host this process is actually talking to: the value from
-- @--host@ when supplied, otherwise @hostname@ from cachix.dhall. Use this
-- instead of @Config.hostname (config env)@ in commands, which ignores @--host@.
effectiveHost :: Env -> URI
effectiveHost = Config.host . cachixoptions

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
