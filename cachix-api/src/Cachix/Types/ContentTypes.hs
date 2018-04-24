{-# LANGUAGE MultiParamTypeClasses #-}
module Cachix.Types.ContentTypes (
  XNixNar,
  XNixNarInfo,
  XNixCacheInfo
) where

import           Data.Typeable      (Typeable)
import qualified Network.HTTP.Media as M
import           Servant.API

import Cachix.Api.Types

data XNixNar deriving Typeable
data XNixNarInfo deriving Typeable
data XNixCacheInfo deriving Typeable

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

instance MimeUnrender XNixNar Nar where
  mimeUnrender _ bs = Right (Nar bs)
