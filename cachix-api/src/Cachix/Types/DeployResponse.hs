module Cachix.Types.DeployResponse where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.HashMap.Strict
import Data.Swagger (ToSchema)
import Data.UUID (UUID)
import Protolude

data Details = Details
  { id :: UUID,
    url :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, NFData)

newtype DeployResponse = DeployResponse
  { agents :: HashMap Text Details
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, NFData)
