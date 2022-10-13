{-# LANGUAGE DataKinds #-}

module Cachix.Types.Deploy where

import Data.HashMap.Strict
import Data.Swagger (ToSchema)
import Deriving.Aeson
import Protolude

data Deploy = Deploy
  { agents :: HashMap Text Text,
    rollbackScript :: Maybe (HashMap Text Text)
  }
  deriving (Show, Eq, Generic, FromJSON, ToSchema)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields] Deploy
