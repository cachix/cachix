module Cachix.DebugInfod.Types
  ( DebugInfoRedirect (..),
    DebugInfodEnv (..),
  )
where

import Cachix.Client.URI (URI)
import Cachix.DebugInfod.Cache (FetcherCache)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Network.HTTP.Client (Manager)
import Protolude
import Servant.Auth.Client (Token)
import Servant.Client.Streaming (ClientEnv)

-- | JSON redirect returned by the binary cache debuginfo endpoint.
-- Points to the NAR archive containing debug symbols.
data DebugInfoRedirect = DebugInfoRedirect
  { -- | Relative path to the compressed NAR (e.g. "nar/xxx.nar.xz")
    archive :: Text,
    -- | Relative path to the file inside the NAR
    member :: Text
  }
  deriving (Show, Eq)

instance FromJSON DebugInfoRedirect where
  parseJSON = withObject "DebugInfoRedirect" $ \o ->
    DebugInfoRedirect
      <$> o
      .: "archive"
      <*> o .: "member"

-- | Runtime environment for the debuginfod server.
data DebugInfodEnv = DebugInfodEnv
  { envCacheBaseUrl :: URI,
    envAuthToken :: Maybe Token,
    envHttpManager :: Manager,
    envClientEnv :: ClientEnv,
    envCacheName :: Text,
    envFetcherCache :: FetcherCache,
    envStoreCache :: FetcherCache
  }
