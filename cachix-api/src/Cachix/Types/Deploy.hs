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
    rollbackScript :: Maybe (HashMap Text Text)
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)
