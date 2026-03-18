module Cachix.DebugInfod.BuildId
  ( BuildId,
    parseBuildId,
    buildIdToDebugPath,
    buildIdToExecutablePath,
    buildIdToSourcePath,
    buildIdToSourceOverlayPath,
    buildIdText,
    FromHttpApiData,
  )
where

import Data.Text qualified as T
import Protolude
import Servant.API (FromHttpApiData (..))

-- | A validated build ID (40 lowercase hex chars, SHA1 format).
newtype BuildId = BuildId Text
  deriving (Show, Eq, Ord)

instance FromHttpApiData BuildId where
  parseUrlPiece = parseBuildId

-- | Parse and validate a build ID string.
-- Must be exactly 40 hex characters.
parseBuildId :: Text -> Either Text BuildId
parseBuildId raw
  | T.length lower /= 40 = Left $ "bad build_id length " <> show (T.length raw)
  | not (T.all isHexDigit lower) = Left "bad character in build_id"
  | otherwise = Right (BuildId lower)
  where
    lower = T.toLower raw

-- | Convert a build ID to a path within a debug output with the given extension.
-- e.g. "483bd7f7..." with ".debug" -> "lib/debug/.build-id/48/3bd7f7....debug"
buildIdPath :: FilePath -> BuildId -> FilePath
buildIdPath ext (BuildId bid) =
  "lib/debug/.build-id/" <> toS (T.take 2 bid) <> "/" <> toS (T.drop 2 bid) <> ext

buildIdToDebugPath :: BuildId -> FilePath
buildIdToDebugPath = buildIdPath ".debug"

buildIdToExecutablePath :: BuildId -> FilePath
buildIdToExecutablePath = buildIdPath ".executable"

buildIdToSourcePath :: BuildId -> FilePath
buildIdToSourcePath = buildIdPath ".source"

buildIdToSourceOverlayPath :: BuildId -> FilePath
buildIdToSourceOverlayPath = buildIdPath ".sourceoverlay"

-- | Extract the text representation.
buildIdText :: BuildId -> Text
buildIdText (BuildId t) = t
