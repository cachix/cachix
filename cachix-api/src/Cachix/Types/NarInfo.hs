{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cachix.Types.NarInfo
  ( CachixNarInfo,
    NarInfo (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Nix.NarInfo (NarInfo (..))
import Protolude

deriving instance Swagger.ToSchema CachixNarInfo

deriving instance OpenApi.ToSchema CachixNarInfo

deriving instance ToJSON CachixNarInfo

deriving instance FromJSON CachixNarInfo

deriving instance NFData CachixNarInfo

type CachixNarInfo = NarInfo Text Text Text
