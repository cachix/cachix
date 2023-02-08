module Cachix.Types.MultipartUpload where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.UUID (UUID)
import Protolude

data CreateMultipartUploadResponse = CreateMultipartUploadResponse
  { narId :: UUID,
    uploadId :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, NFData)

newtype UploadPartResponse = UploadPartResponse {uploadUrl :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, NFData)

data CompletedPart = CompletedPart
  { partNumber :: Int,
    partHash :: Text,
    eTag :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, NFData)

data CompletedMultipartUpload = CompletedMultipartUpload
  { parts :: Maybe (NonEmpty CompletedPart),
    fileHash :: Text,
    fileSize :: Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, NFData)
