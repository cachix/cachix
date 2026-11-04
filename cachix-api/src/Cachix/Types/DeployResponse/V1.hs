module Cachix.Types.DeployResponse.V1 where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.HashMap.Strict
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Protolude

newtype DeployResponse = DeployResponse
  { agents :: HashMap Text Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Swagger.ToSchema, NFData)

instance OpenApi.ToSchema DeployResponse where
  declareNamedSchema proxy = do
    OpenApi.NamedSchema _ responseSchema <- OpenApi.genericDeclareNamedSchema OpenApi.defaultSchemaOptions proxy
    pure $ OpenApi.NamedSchema (Just "DeployResponseV1") responseSchema
