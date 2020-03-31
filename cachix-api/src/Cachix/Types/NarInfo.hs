{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cachix.Types.NarInfo
  ( CachixNarInfo,
    NarInfo (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Nix.NarInfo (NarInfo (..))

deriving instance ToSchema CachixNarInfo

deriving instance ToJSON CachixNarInfo

deriving instance FromJSON CachixNarInfo

type CachixNarInfo = NarInfo Text Text Text
