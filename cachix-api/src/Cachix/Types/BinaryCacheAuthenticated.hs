module Cachix.Types.BinaryCacheAuthenticated
  ( BinaryCacheAuthenticated(..)
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import Data.Swagger
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


-- | Binary Cache response content when user is authenticated
data BinaryCacheAuthenticated = BinaryCacheAuthenticated
  { name :: Text
  , uri :: Text
  , publicSigningKeys :: [Text]
  , totalFileSize :: Integer
  } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
