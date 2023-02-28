module Cachix.Types.MultipartUpload where

import Cachix.Types.NarInfoCreate (NarInfoCreate)
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

-- | Any hashes or headers required to create the presigned URL.
data SigningData = SigningData
  { contentMD5 :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, NFData)

newtype UploadPartResponse = UploadPartResponse {uploadUrl :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, NFData)

data CompletedPart = CompletedPart
  { partNumber :: Int,
    -- | An opaque identifier for the uploaded part.
    eTag :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, NFData)

type CompletedParts = Maybe (NonEmpty CompletedPart)

data CompletedMultipartUpload = CompletedMultipartUpload
  { -- | A list of 'CompletedPart`, sorted by the 'partNumber'.
    parts :: CompletedParts,
    -- | The narinfo to create after verifying the upload.
    narInfoCreate :: NarInfoCreate
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
