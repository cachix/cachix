module Cachix.Types.PinRevision where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Data.Time (UTCTime)
import Protolude

data PinRevision = PinRevision
  { storePath :: Text,
    revision :: Integer,
    createdOn :: UTCTime,
    artifacts :: [Text],
    -- | True when the narinfo for this revision's store_hash has been
    -- deleted (Clear, GC). storePath then lacks the human-readable name
    -- suffix and the artifact links will 404.
    orphaned :: Bool
  }
  deriving (Show, Eq, Generic, NFData, FromJSON, ToJSON, Swagger.ToSchema, OpenApi.ToSchema)
