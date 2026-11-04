module Cachix.Types.Permission where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Protolude

data Permission = Read | Write | Admin
  deriving (Generic, Show, FromJSON, ToJSON, Swagger.ToSchema, OpenApi.ToSchema, NFData, Eq, Ord)
