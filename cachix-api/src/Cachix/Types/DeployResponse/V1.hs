module Cachix.Types.DeployResponse.V1 where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.HashMap.Strict
import Data.Swagger (ToSchema)
import Protolude

newtype DeployResponse = DeployResponse
  { agents :: HashMap Text Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, NFData)
