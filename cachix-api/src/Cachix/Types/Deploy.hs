{-# LANGUAGE DeriveAnyClass #-}

module Cachix.Types.Deploy where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.HashMap.Strict
import Data.Swagger (ToSchema)
import Protolude

newtype Deploy = Deploy
  { agents :: HashMap Text Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
