module Cachix.Types.DeployResponse.V2 where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.HashMap.Strict
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Data.UUID (UUID)
import Protolude

data Details = Details
  { id :: UUID,
    url :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Swagger.ToSchema, OpenApi.ToSchema, NFData)

newtype DeployResponse = DeployResponse
  { agents :: HashMap Text Details
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Swagger.ToSchema, NFData)

instance OpenApi.ToSchema DeployResponse where
  declareNamedSchema proxy = do
    OpenApi.NamedSchema _ responseSchema <- OpenApi.genericDeclareNamedSchema OpenApi.defaultSchemaOptions proxy
    pure $ OpenApi.NamedSchema (Just "DeployResponseV2") responseSchema
