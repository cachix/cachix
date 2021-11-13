{-# LANGUAGE DeriveAnyClass #-}

module Cachix.Types.DeployResponse where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.HashMap.Strict
import Data.Swagger (ToSchema)
import Protolude

newtype DeployResponse = DeployResponse
  { agents :: HashMap Text Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema, NFData)
