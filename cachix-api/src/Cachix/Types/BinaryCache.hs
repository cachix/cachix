module Cachix.Types.BinaryCache where

import Cachix.Types.Permission (Permission)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Protolude
import Servant.API
import Text.Read

type BinaryCacheName = Text

data BinaryCache = BinaryCache
  { name :: Text,
    uri :: Text,
    isPublic :: Bool,
    publicSigningKeys :: [Text],
    githubUsername :: Text,
    permission :: Permission,
    preferredCompressionMethod :: CompressionMethod
  }
  deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema, OpenApi.ToSchema, NFData)

data CompressionMethod = XZ | ZSTD
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Swagger.ToSchema, OpenApi.ToSchema, NFData)

instance Read CompressionMethod where
  readsPrec _ value =
    case map toUpper value of
      "XZ" -> [(XZ, "")]
      "ZST" -> [(ZSTD, "")]
      "ZSTD" -> [(ZSTD, "")]
      _ -> []

instance FromHttpApiData CompressionMethod where
  parseUrlPiece "xz" = Right XZ
  parseUrlPiece "zst" = Right ZSTD
  parseUrlPiece compressionMethod = Left $ "Wrong compression method: " <> compressionMethod

instance ToHttpApiData CompressionMethod where
  toUrlPiece XZ = "xz"
  toUrlPiece ZSTD = "zst"

instance Swagger.ToParamSchema CompressionMethod where
  toParamSchema _ = Swagger.toParamSchema (Proxy :: Proxy Text)

instance OpenApi.ToParamSchema CompressionMethod where
  toParamSchema _ = OpenApi.toParamSchema (Proxy :: Proxy Text)
