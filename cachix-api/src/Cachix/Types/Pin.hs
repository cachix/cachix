module Cachix.Types.Pin where

import qualified Cachix.Types.PinCreate as PinCreate
import qualified Cachix.Types.PinRevision as PinRevision
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Data.Time (UTCTime)
import Protolude

data Pin = Pin
  { name :: Text,
    createdOn :: UTCTime,
    keep :: PinCreate.Keep,
    lastRevision :: PinRevision.PinRevision
  }
  deriving (Show, Eq, Generic, NFData, FromJSON, ToJSON, Swagger.ToSchema, OpenApi.ToSchema)
