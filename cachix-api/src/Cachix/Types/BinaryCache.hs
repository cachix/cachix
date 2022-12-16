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
    preferredCompressionMethod :: CompressionMethod
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema, NFData)

data CompressionMethod = XZ | ZSTD
  deriving (Show, Read, Generic, FromJSON, ToJSON, ToSchema, NFData)

instance FromHttpApiData CompressionMethod where
  parseUrlPiece "xz" = Right XZ
  parseUrlPiece "zst" = Right ZSTD
  parseUrlPiece compressionMethod = Left $ "Wrong compression method: " <> compressionMethod

instance ToHttpApiData CompressionMethod where
  toUrlPiece XZ = "xz"
  toUrlPiece ZSTD = "zst"

instance ToParamSchema CompressionMethod where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)
