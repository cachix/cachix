module Cachix.Types.BinaryCacheCreate
  ( BinaryCacheCreate(..)
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Swagger
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


data BinaryCacheCreate = BinaryCacheCreate
  { publicSigningKey :: Text
  , isPublic :: Bool
  } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
