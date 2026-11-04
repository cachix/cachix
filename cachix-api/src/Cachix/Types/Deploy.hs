{-# LANGUAGE DataKinds #-}

module Cachix.Types.Deploy where

import Data.HashMap.Strict
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Deriving.Aeson
import Protolude

data Deploy = Deploy
  { agents :: HashMap Text Text,
    rollbackScript :: Maybe (HashMap Text Text)
  }
  deriving (Show, Eq, Generic, FromJSON, Swagger.ToSchema, OpenApi.ToSchema)
  deriving (ToJSON) via CustomJSON '[OmitNothingFields] Deploy
