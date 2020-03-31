module Cachix.Types.ByteStringStreaming where

import qualified Data.ByteString.Lazy as LBS
import Data.Swagger (NamedSchema (..), ToSchema (..), binarySchema)
import Protolude

newtype ByteStringStreaming = ByteStringStreaming ByteString

instance ToSchema ByteStringStreaming where
  declareNamedSchema _ = pure $ NamedSchema (Just "ByteString") binarySchema

newtype LazyByteStringStreaming = LazyByteStringStreaming LBS.ByteString

instance ToSchema LazyByteStringStreaming where
  declareNamedSchema _ = pure $ NamedSchema (Just "lazyByteString") binarySchema
