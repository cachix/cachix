module Cachix.Types.CreateToken
  ( CreateToken (..),
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Swagger
import Protolude

data CreateToken
  = CreateToken
      { description :: Text
      }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
