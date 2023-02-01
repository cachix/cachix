{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.API
  ( BinaryCacheAPI (..),
    API,
    CachixAuth,
  )
where

import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Cachix.Types.ByteStringStreaming as ByteStringStreaming
import Cachix.Types.ContentTypes
import Cachix.Types.CreateNarResponse (CreateNarResponse)
import qualified Cachix.Types.MultipartUpload as Multipart
import Cachix.Types.NarFileName (NarFileName (..))
import qualified Cachix.Types.NarInfo as NarInfo
import qualified Cachix.Types.NarInfoCreate as NarInfoCreate
import qualified Cachix.Types.NarInfoHash as NarInfoHash
import qualified Cachix.Types.NixCacheInfo as NixCacheInfo
import Cachix.Types.Servant (Get302, Head, Post302)
import Cachix.Types.Session (Session)
import qualified Cachix.Types.SigningKeyCreate as SigningKeyCreate
import Control.Monad.Trans.Resource
import Crypto.Hash (Digest, MD5)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Conduit (ConduitT)
import Data.UUID (UUID)
import Protolude
import Servant.API hiding (BasicAuth)
import Servant.API.Generic
import Servant.Auth

type CachixAuth = Auth '[Cookie, JWT, BasicAuth] Session

-- Nix CLI + Cachix CLI
data BinaryCacheAPI route = BinaryCacheAPI
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
        :> Get '[XNixNarInfo, JSON] (Headers '[Header "Cache-Control" Text] NarInfo.CachixNarInfo),
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
        :> Get302 '[XNixNar] '[],
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
        :- Summary "Create an empty NAR and initiate a multipart upload"
        :> CachixAuth
        :> "cache"
        :> Capture "name" Text
        :> "nar"
        :> QueryParam "compression" BinaryCache.CompressionMethod
        :> QueryFlag "uploads"
        :> Post '[JSON] CreateNarResponse, -- UploadId
    createAndUploadNar ::
      route
        :- Summary "Upload a NAR directly to the Cachix Server"
        :> Description "This is a legacy API for older Cachix clients. Use 'createNar' instead."
        :> CachixAuth
        :> "cache"
        :> Capture "name" Text
        :> "nar"
        :> QueryParam "compression" BinaryCache.CompressionMethod
        :> StreamBody NoFraming XNixNar (ConduitT () ByteStringStreaming.ByteStringStreaming (ResourceT IO) ())
        :> Post '[JSON] NoContent,
    uploadNarPart ::
      route
        :- Summary "Upload a part of a multipart NAR"
        :> CachixAuth
        :> "cache"
        :> Capture "name" Text
        :> "nar"
        :> Capture "narId" UUID
        :> QueryParam' '[Required] "uploadId" Text
        :> QueryParam' '[Required] "partNumber" Int
        :> Header' '[Required] "Content-Length" Int
        :> Header' '[Required] "Content-MD5" (Digest MD5)
        :> Get '[JSON] (Headers '[Header "Location" Text] NoContent),
    completeNarUpload ::
      route
        :- Summary "Verify the hash digests of each uploaded NAR part"
        :> CachixAuth
        :> "cache"
        :> Capture "name" Text
        :> "nar"
        :> Capture "narId" UUID
        :> QueryParam' '[Required] "uploadId" Text
        :> ReqBody '[JSON] Multipart.CompletedMultipartUpload
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
        :> Get '[XNixNar] (Headers '[Header "X-Content-Type-Options" Text, Header "Cache-Control" Text] ByteStringStreaming.LazyByteStringStreaming),
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
