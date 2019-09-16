module Cachix.Types.SigningKeyCreate
  ( SigningKeyCreate (..)
    )
where

import Data.Aeson
  ( FromJSON,
    ToJSON
    )
import Data.Swagger
import Protolude

-- | Conveys that a signing secret key was created, by sharing the public key.
newtype SigningKeyCreate
  = SigningKeyCreate
      { publicKey :: Text
        }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
