module Cachix.Types.Realisation
  ( Realisation (..),
    splitDrvOutputId,
    mkDrvOutputId,
  )
where

import Data.Aeson
  ( FromJSON (..),
    ToJSON (..),
    object,
    withObject,
    (.:),
    (.=),
  )
import Data.Swagger (ToSchema)
import qualified Data.Text as T
import Protolude

-- | A Nix realisation mapping a derivation output to its content-addressed store path.
--
-- JSON format matches what Nix expects at @realisations\/\<id\>.doi@:
--
-- @
-- { "id": "sha256:\<hash\>!\<outputName\>"
-- , "outPath": "\<storePathBaseName\>"
-- , "signatures": [...]
-- , "dependentRealisations": { "sha256:\<hash\>!\<out\>": "\<storePathBaseName\>", ... }
-- }
-- @
data Realisation = Realisation
  { -- | Derivation output id, e.g. "sha256:<hash>!<outputName>"
    rId :: Text,
    -- | Store path base name of the output
    rOutPath :: Text,
    -- | Signatures for this realisation
    rSignatures :: [Text],
    -- | Map from dependent derivation output ids to their store path base names
    rDependentRealisations :: Map Text Text
  }
  deriving (Eq, Show, Generic, NFData, ToSchema)

-- | Split a derivation output id "sha256:<hash>!<outputName>" into (drvHash, outputName).
-- Returns Nothing if there is no "!" separator.
splitDrvOutputId :: Text -> Maybe (Text, Text)
splitDrvOutputId t =
  case T.breakOn "!" t of
    (_, rest) | T.null rest -> Nothing
    (drvHash, rest) -> Just (drvHash, T.drop 1 rest)

-- | Construct a derivation output id from drvHash and outputName.
mkDrvOutputId :: Text -> Text -> Text
mkDrvOutputId drvHash outputName = drvHash <> "!" <> outputName

instance ToJSON Realisation where
  toJSON Realisation {..} =
    object
      [ "id" .= rId,
        "outPath" .= rOutPath,
        "signatures" .= rSignatures,
        "dependentRealisations" .= rDependentRealisations
      ]

instance FromJSON Realisation where
  parseJSON = withObject "Realisation" $ \o ->
    Realisation
      <$> o
      .: "id"
      <*> o .: "outPath"
      <*> o .: "signatures"
      <*> o .: "dependentRealisations"
