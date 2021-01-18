module Cachix.Types.NixCacheInfo where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Protolude

data NixCacheInfo
  = NixCacheInfo
      { storeDir :: Text,
        wantMassQuery :: Integer,
        priority :: Integer
      }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema, NFData)
