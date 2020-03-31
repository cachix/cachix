{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.API
  ( api,
    BinaryCacheAPI (..),
    API,
    CachixAuth,
  )
where

import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Cachix.Types.ByteStringStreaming as ByteStringStreaming
import Cachix.Types.ContentTypes
import Cachix.Types.NarFileName (NarFileName (..))
import qualified Cachix.Types.NarInfo as NarInfo
import qualified Cachix.Types.NarInfoCreate as NarInfoCreate
import qualified Cachix.Types.NarInfoHash as NarInfoHash
import qualified Cachix.Types.NixCacheInfo as NixCacheInfo
import Cachix.Types.Servant (Head)
import Cachix.Types.Session (Session)
import qualified Cachix.Types.SigningKeyCreate as SigningKeyCreate
import Control.Monad.Trans.Resource
import Data.Conduit (ConduitT)
import Protolude
import Servant.API hiding (BasicAuth)
import Servant.API.Generic
import Servant.Auth

type CachixAuth = Auth '[Cookie, JWT, BasicAuth] Session

-- Nix CLI + Cachix CLI
data BinaryCacheAPI route
  = BinaryCacheAPI
      { -- https://cache.nixos.org/nix-cache-info
        nixCacheInfo ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> "nix-cache-info"
            :> Get '[XNixCacheInfo, JSON] NixCacheInfo.NixCacheInfo,
        -- Hydra: src/lib/Hydra/View/NARInfo.pm
        narinfo ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> Capture "narinfohash" NarInfoHash.NarInfoHash
            :> Get '[XNixNarInfo, JSON] NarInfo.CachixNarInfo,
        narinfoHead ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> Capture "narinfohash" NarInfoHash.NarInfoHash
            :> Head,
        -- Hydra: src/lib/Hydra/View/NixNAR.pm
        nar ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> "nar"
            :> Capture "nar" NarFileName
            :> StreamGet NoFraming XNixNar (ConduitT () ByteStringStreaming.ByteStringStreaming (ResourceT IO) ()),
        -- cachix specific
        getCache ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> Get '[JSON] BinaryCache.BinaryCache,
        narinfoBulk ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> "narinfo"
            :> Summary "Given a list of store hashes, return a list of those that are missing"
            :> ReqBody '[JSON] [Text]
            :> Post '[JSON] [Text],
        narURL ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> "narurl"
            :> Capture "nar" NarFileName
            :> Get '[JSON] Text,
        createNarinfo ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> Capture "narinfohash" NarInfoHash.NarInfoHash
            :> ReqBody '[JSON] NarInfoCreate.NarInfoCreate
            :> Post '[JSON] NoContent,
        createNar ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> "nar"
            :> StreamBody NoFraming XNixNar (ConduitT () ByteStringStreaming.ByteStringStreaming (ResourceT IO) ())
            :> Post '[JSON] NoContent,
        serveNarContent ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> "serve"
            :> Capture "storehash" Text
            :> CaptureAll "filepath" Text
            :> Summary "Serve a file from a given store path"
            :> Get '[XNixNar] (Headers '[Header "X-Content-Type-Options" Text] ByteStringStreaming.LazyByteStringStreaming),
        createKey ::
          route
            :- CachixAuth
            :> "cache"
            :> Capture "name" Text
            :> "key"
            :> ReqBody '[JSON] SigningKeyCreate.SigningKeyCreate
            :> Post '[JSON] NoContent
      }
  deriving (Generic)

type API = "api" :> "v1" :> ToServantApi BinaryCacheAPI

api :: Proxy API
api = Proxy
