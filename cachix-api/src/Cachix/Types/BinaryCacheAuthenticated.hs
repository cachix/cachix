module Cachix.Types.BinaryCacheAuthenticated
  ( BinaryCacheAuthenticated (..),
  )
where

import Cachix.Types.Permission (Permission)
import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Swagger
import Protolude

-- | Binary Cache response content when user is authenticated
data BinaryCacheAuthenticated
  = BinaryCacheAuthenticated
      { name :: Text,
        uri :: Text,
        publicSigningKeys :: [Text],
        isPublic :: Bool,
        totalFileSize :: Integer,
        permission :: Permission
      }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
