module Cachix.Types.Deployment where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Protolude

data Status
  = Pending
  | InProgress
  | Cancelled
  | Failed
  | Succeeded
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON, Swagger.ToSchema, OpenApi.ToSchema, NFData)

data Deployment = Deployment
  { id :: UUID,
    index :: Int64,
    createdOn :: UTCTime,
    startedOn :: Maybe UTCTime,
    finishedOn :: Maybe UTCTime,
    storePath :: Text,
    closureSize :: Maybe Int64,
    status :: Status
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Swagger.ToSchema, OpenApi.ToSchema, NFData)
