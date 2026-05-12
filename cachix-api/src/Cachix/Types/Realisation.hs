module Cachix.Types.Realisation
  ( DrvOutput (..),
    UnkeyedRealisation (..),
    Realisation (..),
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
import Protolude

-- | A build-trace key: a derivation store path and one of its output names.
--
-- Matches Nix's @DrvOutput@ in the @build-trace-v2@ wire format. @drvPath@ is
-- the basename of the derivation store path (e.g. @abc...-foo.drv@), not a hash.
data DrvOutput = DrvOutput
  { drvPath :: Text,
    outputName :: Text
  }
  deriving (Eq, Ord, Show, Generic, NFData, ToSchema)

-- | The body of a realisation: which store path this output resolved to, plus signatures.
data UnkeyedRealisation = UnkeyedRealisation
  { outPath :: Text,
    signatures :: [Text]
  }
  deriving (Eq, Show, Generic, NFData, ToSchema)

-- | A full realisation entry as served at
-- @build-trace-v2\/\<drvName\>\/\<outputName\>.doi@.
--
-- JSON wire format:
--
-- @
-- { "key":   { "drvPath": "abc...-foo.drv", "outputName": "out" }
-- , "value": { "outPath": "xyz...-foo",      "signatures": ["..."] }
-- }
-- @
data Realisation = Realisation
  { key :: DrvOutput,
    value :: UnkeyedRealisation
  }
  deriving (Eq, Show, Generic, NFData, ToSchema)

instance ToJSON DrvOutput where
  toJSON DrvOutput {drvPath, outputName} =
    object ["drvPath" .= drvPath, "outputName" .= outputName]

instance FromJSON DrvOutput where
  parseJSON = withObject "DrvOutput" $ \o ->
    DrvOutput <$> o .: "drvPath" <*> o .: "outputName"

instance ToJSON UnkeyedRealisation where
  toJSON UnkeyedRealisation {outPath, signatures} =
    object ["outPath" .= outPath, "signatures" .= signatures]

instance FromJSON UnkeyedRealisation where
  parseJSON = withObject "UnkeyedRealisation" $ \o ->
    UnkeyedRealisation <$> o .: "outPath" <*> o .: "signatures"

instance ToJSON Realisation where
  toJSON Realisation {key, value} =
    object ["key" .= key, "value" .= value]

instance FromJSON Realisation where
  parseJSON = withObject "Realisation" $ \o ->
    Realisation <$> o .: "key" <*> o .: "value"
