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
import qualified Cachix.Types.NarInfo as NarInfo
import qualified Cachix.Types.NixCacheInfo as NixCacheInfo
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
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

instance MimeRender PlainText ByteStringStreaming.ByteStringStreaming where
  mimeRender _ = BSL.fromStrict . coerce

instance MimeUnrender PlainText ByteStringStreaming.ByteStringStreaming where
  mimeUnrender _ = Right . coerce . BSL.toStrict

instance (Accept cts) => MimeUnrender cts NoContent where
  mimeUnrender _ "" = Right NoContent
  mimeUnrender _ _ = Left "Unexpected content"

instance NFData (WithStatus sts NoContent) where
  rnf (WithStatus status) = rnf status

instance NFData (WithStatus sts (Headers '[Header "Location" Text] NoContent)) where
  rnf (WithStatus status) = rnf status
