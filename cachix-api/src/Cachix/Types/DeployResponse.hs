module Cachix.Types.DeployResponse where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.HashMap.Strict
import Data.Swagger (ToSchema)
import Data.UUID (UUID)
import Protolude

data Status
  = Pending
  | InProgress
  | Cancelled
  | Failed
  | Succeeded
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, NFData)

data Details = Details
  { id :: UUID,
    status :: Status,
    url :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, NFData)

newtype DeployResponse = DeployResponse
  { agents :: HashMap Text Details
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, NFData)
