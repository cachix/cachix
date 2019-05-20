module Cachix.Formats.CachixSigningKey where

import           Prelude
import           Data.Text                      ( Text )
import           Data.Aeson
import           Cachix.Formats.Common

-- | An object representing a key for signing nars and pushing to a cache,
-- including "kind" field and cache name for context when serialized.
--
-- These are created by the @cachix export@ command.
data CachixSigningKey = CachixSigningKey
 { cacheName :: Text
 , secretKey :: Text
 } deriving (Eq, Ord)

instance ToJSON CachixSigningKey where
  toJSON a = object
    [ "kind" .= String "CachixSigningKey"
    , "cacheName" .= cacheName a
    , "secretKey" .= secretKey a
    ]
  toEncoding a = pairs
    (  "kind" .= String "CachixSigningKey"
    <> "cacheName" .= cacheName a
    <> "secretKey" .= secretKey a
    )

instance FromJSON CachixSigningKey where
  parseJSON = withKind "CachixSigningKey" $ \o ->
    CachixSigningKey
      <$> o .: "cacheName"
      <*> o .: "secretKey"
