module Cachix.Types.NarFileName 
  (NarFileName(..)) where 

import Protolude
import Servant.API
import Data.Text (dropEnd, takeEnd)

-- | <hash>.nar.xz file
data NarFileName = NarFileName 
  { contentHash :: Text 
  , extension :: Text
  } deriving (Generic)

instance FromHttpApiData NarFileName where

  parseUrlPiece s =
    if takeEnd 7 s == ".nar.xz"
      then Right $ NarFileName (dropEnd 7 s) "xz"
      else Left "Wrong extension"

instance ToHttpApiData NarFileName where

  toUrlPiece narfilename = contentHash narfilename <> ".nar." <> extension narfilename
