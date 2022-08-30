{-# LANGUAGE DeriveAnyClass #-}

module Cachix.Types.Deploy where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.HashMap.Strict
import Data.Swagger (ToSchema)
import Protolude

data Deploy = Deploy
  { agents :: HashMap Text Text,
    rollbackScript :: HashMap Text Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
