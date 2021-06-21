module Cachix.Types.NarFileName
  ( NarFileName (..),
  )
where

import Data.Swagger (ToParamSchema (..))
import qualified Data.Text as T
import Protolude
import Servant.API

-- | <hash>.nar.<extension> file
data NarFileName = NarFileName
  { contentHash :: Text,
    extension :: Text
  }
  deriving (Generic)

instance FromHttpApiData NarFileName where
  parseUrlPiece s =
    case T.splitOn "." s of
      [filename, "nar", ext] ->
        Right $ NarFileName filename ext
      _ -> Left $ "Wrong nar filename: " <> s

instance ToHttpApiData NarFileName where
  toUrlPiece narfilename = contentHash narfilename <> ".nar." <> extension narfilename

instance ToParamSchema NarFileName where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Text)
