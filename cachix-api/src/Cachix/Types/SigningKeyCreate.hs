module Cachix.Types.SigningKeyCreate
  ( SigningKeyCreate(..)
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Swagger
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


newtype SigningKeyCreate = SigningKeyCreate
  { publicKey :: Text
  } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
