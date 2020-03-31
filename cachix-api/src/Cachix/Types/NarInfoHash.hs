module Cachix.Types.NarInfoHash
  ( NarInfoHash (..),
  )
where

import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (dropEnd, takeEnd)
import Protolude
import Servant.API

-- | Store path hash
newtype NarInfoHash = NarInfoHash {unnarinfohash :: Text}
  deriving (Generic, ToSchema, ToParamSchema)

instance FromHttpApiData NarInfoHash where
  parseUrlPiece s =
    if takeEnd 8 s == ".narinfo"
      then Right $ NarInfoHash (dropEnd 8 s)
      else Left ""

instance ToHttpApiData NarInfoHash where
  toUrlPiece (NarInfoHash n) = n <> ".narinfo"
