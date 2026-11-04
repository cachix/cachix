module Cachix.Types.SigningKeyCreate
  ( SigningKeyCreate (..),
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Protolude

-- | Conveys that a signing secret key was created, by sharing the public key.
newtype SigningKeyCreate = SigningKeyCreate
  { publicKey :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, Swagger.ToSchema, OpenApi.ToSchema)
