{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cachix.Types.ContentTypes
  ( XNixNar,
    XNixNarInfo,
    XNixCacheInfo,
  )
where

import qualified Cachix.Types.ByteStringStreaming as ByteStringStreaming
import qualified Cachix.Types.MultipartUpload as MultipartUpload
import qualified Cachix.Types.NarInfo as NarInfo
import qualified Cachix.Types.NixCacheInfo as NixCacheInfo
import Crypto.Hash (Digest, HashAlgorithm, digestFromByteString)
import Data.ByteArray.Encoding
import Data.ByteString.Builder (stringUtf8)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Swagger (ToParamSchema (..), ToSchema)
import qualified Network.HTTP.Media as M
import Protolude
import Servant.API

data XNixNar deriving (Typeable)

data XNixNarInfo deriving (Typeable)

data XNixCacheInfo deriving (Typeable)

instance Accept XNixCacheInfo where
  contentType _ = "application" M.// "octet-stream"

instance Accept XNixNarInfo where
  contentType _ = "text" M.// "x-nix-narinfo"

instance Accept XNixNar where
  contentType _ = "application" M.// "octet-stream"

instance MimeUnrender XNixCacheInfo NixCacheInfo.NixCacheInfo where
  mimeUnrender _ _ = Left "TODO"

instance MimeUnrender XNixNarInfo NarInfo.CachixNarInfo where
  mimeUnrender _ _ = Left "TODO"

instance MimeRender XNixNar ByteStringStreaming.ByteStringStreaming where
  mimeRender _ = BSL.fromStrict . coerce

instance MimeUnrender XNixNar ByteStringStreaming.ByteStringStreaming where
  mimeUnrender _ = Right . coerce . BSL.toStrict

instance MimeUnrender XNixNar ByteStringStreaming.LazyByteStringStreaming where
  mimeUnrender _ = Right . coerce

instance MimeRender XNixNar ByteStringStreaming.LazyByteStringStreaming where
  mimeRender _ = coerce

instance MimeUnrender XNixNar ByteString where
  mimeUnrender _ = Right . BSL.toStrict

instance MimeRender XNixNar ByteString where
  mimeRender _ = BSL.fromStrict

instance HashAlgorithm a => ToHttpApiData (Digest a) where
  toHeader = convertToBase Base16

instance HashAlgorithm a => FromHttpApiData (Digest a) where
  parseHeader encoded = do
    bs <- first toS (convertFromBase Base16 encoded)
    maybeToEither "Invalid hash digest" $ digestFromByteString (bs :: ByteString)

instance ToParamSchema (Digest a) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)
