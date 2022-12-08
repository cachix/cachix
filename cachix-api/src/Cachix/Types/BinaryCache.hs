module Cachix.Types.BinaryCache where

import Cachix.Types.Permission (Permission)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToParamSchema (..), ToSchema)
import Protolude
import Servant.API

data BinaryCache = BinaryCache
  { name :: Text,
    uri :: Text,
    isPublic :: Bool,
    publicSigningKeys :: [Text],
    githubUsername :: Text,
    permission :: Permission,
    compression :: CompressionMode
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema, NFData)

data CompressionMode = XZ | ZSTD
  deriving (Show, Read, Generic, FromJSON, ToJSON, ToSchema, NFData)

instance FromHttpApiData CompressionMode where
  parseUrlPiece "xz" = Right XZ
  parseUrlPiece "zst" = Right ZSTD
  parseUrlPiece compressionMode = Left $ "Wrong compression mode: " <> compressionMode

instance ToHttpApiData CompressionMode where
  toUrlPiece XZ = "xz"
  toUrlPiece ZSTD = "zst"

instance ToParamSchema CompressionMode where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)
