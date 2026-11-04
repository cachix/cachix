module Cachix.Types.NarInfoHash
  ( NarInfoHash (..),
  )
where

import qualified Data.OpenApi as OpenApi
import qualified Data.Swagger as Swagger
import Data.Text (dropEnd, takeEnd)
import Protolude
import Servant.API

-- | Store path hash
newtype NarInfoHash = NarInfoHash {unnarinfohash :: Text}
  deriving (Generic, Swagger.ToSchema, Swagger.ToParamSchema, OpenApi.ToSchema, OpenApi.ToParamSchema)

instance FromHttpApiData NarInfoHash where
  parseUrlPiece s =
    if takeEnd 8 s == ".narinfo"
      then Right $ NarInfoHash (dropEnd 8 s)
      else Left ""

instance ToHttpApiData NarInfoHash where
  toUrlPiece (NarInfoHash n) = n <> ".narinfo"
