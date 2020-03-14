{-# LANGUAGE MultiParamTypeClasses #-}

module Cachix.Types.ContentTypes
  ( XNixNar,
    XNixNarInfo,
    XNixCacheInfo,
  )
where

import Cachix.Api.Types
import qualified Data.ByteString.Lazy as BSL
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
  contentType _ = "application" M.// "x-nix-nar"

instance MimeUnrender XNixCacheInfo NixCacheInfo where
  mimeUnrender _ _ = Left "TODO"

instance MimeUnrender XNixNarInfo NarInfo where
  mimeUnrender _ _ = Left "TODO"

instance MimeRender XNixNar ByteString where
  mimeRender _ = BSL.fromStrict

instance MimeUnrender XNixNar ByteString where
  mimeUnrender _ = Right . BSL.toStrict
