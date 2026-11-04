module Cachix.Types.ByteStringStreaming where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Protolude

newtype ByteStringStreaming = ByteStringStreaming ByteString

instance Swagger.ToSchema ByteStringStreaming where
  declareNamedSchema _ = pure $ Swagger.NamedSchema (Just "ByteString") Swagger.binarySchema

newtype LazyByteStringStreaming = LazyByteStringStreaming LBS.ByteString

instance Swagger.ToSchema LazyByteStringStreaming where
  declareNamedSchema _ = pure $ Swagger.NamedSchema (Just "lazyByteString") Swagger.binarySchema

instance OpenApi.ToSchema ByteStringStreaming where
  declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "ByteString") OpenApi.binarySchema

instance OpenApi.ToSchema LazyByteStringStreaming where
  declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "lazyByteString") OpenApi.binarySchema
