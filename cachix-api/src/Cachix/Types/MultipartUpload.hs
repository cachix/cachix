module Cachix.Types.MultipartUpload where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToParamSchema (..), ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import Protolude
import Servant.API

data CreateMultipartUpload = CreateMultipartUpload
  { key :: UUID,
    uploadId :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, NFData)

data CompletedPart = CompletedPart
  { partNumber :: Int,
    eTag :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, NFData)

data CompletedMultipartUpload = CompletedMultipartUpload
  { parts :: Maybe (NonEmpty CompletedPart)
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema, NFData)
