{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cachix.Api.NarInfo
  ( SimpleNarInfo,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Nix.NarInfo (SimpleNarInfo)

deriving instance ToSchema SimpleNarInfo

deriving instance ToJSON SimpleNarInfo

deriving instance FromJSON SimpleNarInfo
