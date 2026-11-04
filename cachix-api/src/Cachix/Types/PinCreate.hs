module Cachix.Types.PinCreate where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Protolude

data Keep = Days Int | Revisions Int | Forever
  deriving (Show, Eq, Generic, ToJSON, FromJSON, OpenApi.ToSchema)

-- to support Keep with data constructor with arguments
instance Swagger.ToSchema Keep where
  declareNamedSchema = Swagger.genericDeclareNamedSchemaUnrestricted Swagger.defaultSchemaOptions

data PinCreate = PinCreate
  { name :: Text,
    storePath :: Text,
    artifacts :: [Text],
    keep :: Maybe Keep
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Swagger.ToSchema, OpenApi.ToSchema)
