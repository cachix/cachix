module Cachix.Types.NixCacheInfo where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Protolude

data NixCacheInfo = NixCacheInfo
  { storeDir :: Text,
    wantMassQuery :: Integer,
    priority :: Integer
  }
  deriving (Generic, Show, FromJSON, ToJSON, Swagger.ToSchema, OpenApi.ToSchema, NFData)
