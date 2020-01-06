module Cachix.Types.NarInfoCreate
  ( NarInfoCreate (..),
    NarInfoInvalid,
    isNarInfoCreateValid,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Swagger
import Protolude

-- TODO: get rid of c prefix

-- | Client create type
data NarInfoCreate
  = NarInfoCreate
      { -- | \$storePrefix / $storeHash - $storeSuffix
        cStoreHash :: Text,
        -- | \$storePrefix / $storeHash - $storeSuffix
        cStoreSuffix :: Text,
        cNarHash :: Text,
        cNarSize :: Integer,
        cFileHash :: Text,
        cFileSize :: Integer,
        cReferences :: [Text],
        cDeriver :: Text,
        cSig :: Text
      }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data NarInfoInvalid
  = NarSizeIsZero
  deriving (Show, Exception)

isNarInfoCreateValid :: NarInfoCreate -> Either NarInfoInvalid ()
isNarInfoCreateValid nic
  | cNarSize nic == 0 = Left NarSizeIsZero
  | otherwise = Right ()
