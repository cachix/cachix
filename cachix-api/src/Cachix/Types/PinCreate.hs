module Cachix.Types.PinCreate where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema (..), defaultSchemaOptions, genericDeclareNamedSchemaUnrestricted)
import Protolude

data Keep = Days Int | Revisions Int | Forever
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- to support Keep with data constructor with arguments
instance ToSchema Keep where
  declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

data PinCreate = PinCreate
  { name :: Text,
    storePath :: Text,
    artifacts :: [Text],
    keep :: Maybe Keep
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
