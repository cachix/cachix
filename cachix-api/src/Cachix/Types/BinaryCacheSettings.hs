module Cachix.Types.BinaryCacheSettings
  ( BinaryCacheSettings (..),
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Swagger
import Protolude

data BinaryCacheSettings
  = BinaryCacheSettings
      { enableUpstreamNixOSCache :: Bool,
        isPublic :: Bool
      }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
