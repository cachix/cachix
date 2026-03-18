module Cachix.Types.DebugInfo
  ( DebugInfoEntry (..),
    DebugInfoRedirect (..),
    dropRedirectPrefix,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericParseJSON, genericToJSON)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Swagger (ToSchema)
import Protolude

-- | A debug info entry discovered during NAR push.
-- Maps a build ID to its member path inside the NAR.
data DebugInfoEntry = DebugInfoEntry
  { buildId :: Text,
    member :: Text
  }
  deriving (Generic, Show, Eq, NFData, FromJSON, ToJSON, ToSchema)

-- | Redirect response for a debug info lookup.
-- Points to the NAR archive and the file path inside it.
data DebugInfoRedirect = DebugInfoRedirect
  { redirectArchive :: Text,
    redirectMember :: Text
  }
  deriving (Generic, Show, Eq, NFData, ToSchema)

debugInfoRedirectOptions :: Aeson.Options
debugInfoRedirectOptions =
  defaultOptions
    { Aeson.fieldLabelModifier = dropRedirectPrefix
    }

-- | Drop "redirect" prefix and lowercase first char.
-- Exported for use in elm-bridge TH splices.
dropRedirectPrefix :: [Char] -> [Char]
dropRedirectPrefix s = maybe s lowercaseFirst (stripPrefix "redirect" s)
  where
    lowercaseFirst [] = []
    lowercaseFirst (c : cs) = toLower c : cs

instance ToJSON DebugInfoRedirect where
  toJSON = genericToJSON debugInfoRedirectOptions

instance FromJSON DebugInfoRedirect where
  parseJSON = genericParseJSON debugInfoRedirectOptions
