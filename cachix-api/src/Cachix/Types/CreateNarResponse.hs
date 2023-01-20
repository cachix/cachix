module Cachix.Types.CreateNarResponse where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import Protolude
import Servant.API

data CreateNarResponse = CreateNarResponse
  { narId :: UUID,
    uploadId :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, NFData)
