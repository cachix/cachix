module Cachix.Types.Deployment where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Swagger (ToSchema)
import Data.Time.Clock (UTCTime)
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

data Deployment = Deployment
  { id :: UUID,
    index :: Int64,
    startedOn :: Maybe UTCTime,
    createdOn :: UTCTime,
    storePath :: Text,
    closureSize :: Maybe Int64,
    status :: Status
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema, NFData)
