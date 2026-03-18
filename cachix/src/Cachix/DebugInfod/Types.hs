module Cachix.DebugInfod.Types
  ( DebugInfodEnv (..),
    envToken,
  )
where

import Cachix.Client.URI (URI)
import Cachix.DebugInfod.Cache (FetcherCache)
import Network.HTTP.Client (Manager)
import Protolude
import Servant.Auth.Client (Token (..))
import Servant.Client.Streaming (ClientEnv)

-- | Runtime environment for the debuginfod server.
data DebugInfodEnv = DebugInfodEnv
  { envCacheBaseUrl :: URI,
    envAuthToken :: Maybe Token,
    envHttpManager :: Manager,
    envClientEnv :: ClientEnv,
    envCacheName :: Text,
    envFetcherCache :: FetcherCache
  }

-- | Get the auth token, defaulting to empty for unauthenticated requests.
envToken :: DebugInfodEnv -> Token
envToken env = fromMaybe (Token "") (envAuthToken env)
