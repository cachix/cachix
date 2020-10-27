module Cachix.Types.Permission where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Protolude

data Permission = Read | Write | Admin
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, NFData, Eq, Ord)
