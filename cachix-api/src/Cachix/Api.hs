{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Api
  ( servantApi,
    swaggerDoc,
    CachixAPI (..),
    CachixServantAPI,
    CachixAuth,
    InstallAPI (..),
    GitHubAPI (..),
    BinaryCacheAPI (..),
    BinaryCacheStreamingAPI (..),
    BinaryCachStreamingServantAPI,
    module Cachix.Api.Types,
    module Cachix.Types.ContentTypes,
    module Cachix.Types.NarFileName,
  )
where

import Cachix.Api.Types
import qualified Cachix.Types.BinaryCacheAuthenticated as BinaryCacheAuthenticated
import qualified Cachix.Types.BinaryCacheCreate as BinaryCacheCreate
import Cachix.Types.ContentTypes
import qualified Cachix.Types.CreateToken as CreateToken
import qualified Cachix.Types.GitHubTeam as GitHubTeam
import Cachix.Types.NarFileName (NarFileName (..))
import qualified Cachix.Types.NarInfoCreate as NarInfoCreate
import Cachix.Types.Servant (Get302, Head, Post302)
import Cachix.Types.Session (Session)
import qualified Cachix.Types.SigningKeyCreate as SigningKeyCreate
import Cachix.Types.SwaggerOrphans ()
import Control.Lens
import Control.Monad.Trans.Resource
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Conduit (ConduitT)
import Data.Swagger hiding (Header)
import Protolude
import Servant.API hiding (BasicAuth)
import Servant.API.Generic
import Servant.Auth
import Servant.Auth.Swagger ()
import Servant.Swagger
import Web.Cookie (SetCookie)

type CachixAuth = Auth '[Cookie, JWT, BasicAuth] Session

data BinaryCacheAPI route
  = BinaryCacheAPI
      { get ::
          route
            :- CachixAuth
            :> Get '[JSON] BinaryCache,
        create ::
          route
            :- CachixAuth
            :> ReqBody '[JSON] BinaryCacheCreate.BinaryCacheCreate
            :> Post '[JSON] NoContent,
        delete ::
          route
            :- CachixAuth
            :> Delete '[JSON] NoContent,
        -- https://cache.nixos.org/nix-cache-info
        nixCacheInfo ::
          route
            :- CachixAuth
            :> "nix-cache-info"
            :> Get '[XNixCacheInfo, JSON] NixCacheInfo,
        -- Hydra: src/lib/Hydra/View/NARInfo.pm
        narURL
          :: route
               :- CachixAuth
               :> "narurl"
               :> Capture "nar" NarFileName
               :> Get '[JSON] Text,
        narinfo ::
          route
            :- CachixAuth
            :> Capture "narinfo" NarInfoC
            :> Get '[XNixNarInfo, JSON] NarInfo,
        narinfoHead ::
          route
            :- CachixAuth
            :> Capture "narinfo" NarInfoC
            :> Head,
        narinfoBulk ::
          route
            :- CachixAuth
            :> "narinfo"
            :> Summary "Given a list of store hashes, return a list of those that are missing"
            :> ReqBody '[JSON] [Text]
            :> Post '[JSON] [Text],
        createNarinfo ::
          route
            :- Capture "narinfo" NarInfoC
            :> ReqBody '[JSON] NarInfoCreate.NarInfoCreate
            :> Post '[JSON] NoContent,
        createKey ::
          route
            :- CachixAuth
            :> "key"
            :> ReqBody '[JSON] SigningKeyCreate.SigningKeyCreate
            :> Post '[JSON] NoContent
      }
  deriving (Generic)

-- | Streaming endpoints
data BinaryCacheStreamingAPI route
  = BinaryCacheStreamingAPI
      { -- Hydra: src/lib/Hydra/View/NixNAR.pm
        nar ::
          route
            :- CachixAuth
            :> "nar"
            :> Capture "nar" NarFileName
            :> StreamGet NoFraming OctetStream (ConduitT () ByteString (ResourceT IO) ()),
        createNar ::
          route
            :- "nar"
            :> StreamBody NoFraming XNixNar (ConduitT () ByteString (ResourceT IO) ())
            :> Post '[JSON] NoContent,
        serveNarContent ::
          route
            :- CachixAuth
            :> "serve"
            :> Capture "storehash" Text
            :> CaptureAll "filepath" Text
            :> Summary "Serve a file from a given store path"
            :> Get '[OctetStream] (Headers '[Header "X-Content-Type-Options" Text] BSL.ByteString)
      }
  deriving (Generic)

data InstallAPI route
  = InstallAPI
      { installGetLatest ::
          route
            :- Summary "Redirects to a tarball containing nix expression to build the latest version of cachix cli"
            :> Get302 '[JSON] '[],
        installGetVersion ::
          route
            :- Summary "Redirects to a tarball containing nix expression to build given version of cachix cli"
            :> Capture "version" Text
            :> Get302 '[JSON] '[]
      }
  deriving (Generic)

data GitHubAPI route
  = GitHubAPI
      { githubOrganizations ::
          route
            :- CachixAuth
            :> "orgs"
            :> Get '[JSON] [Text],
        githubTeams ::
          route
            :- CachixAuth
            :> "orgs"
            :> Capture "org" Text
            :> "teams"
            :> Get '[JSON] [GitHubTeam.GitHubTeam]
      }
  deriving (Generic)

data CachixAPI route
  = CachixAPI
      { logout ::
          route
            :- "logout"
            :> CachixAuth
            :> Post302 '[JSON]
                 '[ Header "Set-Cookie" SetCookie,
                    Header "Set-Cookie" SetCookie
                  ],
        login ::
          route
            :- "login"
            :> Get302 '[JSON] '[],
        loginCallback ::
          route
            :- "login"
            :> "callback"
            :> QueryParam "code" Text
            :> QueryParam "state" Text
            :> Get302 '[JSON]
                 '[ Header "Set-Cookie" SetCookie,
                    Header "Set-Cookie" SetCookie
                  ],
        user ::
          route
            :- CachixAuth
            :> "user"
            :> Get '[JSON] User,
        createToken ::
          route
            :- CachixAuth
            :> "token"
            :> ReqBody '[JSON] CreateToken.CreateToken
            :> Post '[JSON] Text,
        caches ::
          route
            :- CachixAuth
            :> "cache"
            :> Get '[JSON] [BinaryCacheAuthenticated.BinaryCacheAuthenticated],
        cache ::
          route
            :- "cache"
            :> Capture "name" Text
            :> ToServantApi BinaryCacheAPI,
        install ::
          route
            :- "install"
            :> ToServantApi InstallAPI,
        github ::
          route
            :- "github"
            :> ToServantApi GitHubAPI
      }
  deriving (Generic)

type CachixServantAPI = "api" :> "v1" :> ToServantApi CachixAPI

type BinaryCachStreamingServantAPI =
  "api" :> "v1" :> "cache" :> Capture "name" Text :> ToServantApi BinaryCacheStreamingAPI

servantApi :: Proxy CachixServantAPI
servantApi = Proxy

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger servantApi
    & info
    . title
    .~ "cachix.org API"
    & info
    . version
    .~ "1.0"
    & info
    . description
    ?~ "TODO"
