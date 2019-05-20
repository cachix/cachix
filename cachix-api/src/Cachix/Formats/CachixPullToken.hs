module Cachix.Formats.CachixPullToken where

import           Prelude
import           Data.Text                      ( Text )
import           Data.Aeson
import           Cachix.Formats.Common

-- | An object representing a token for pulling from a cache, including "kind"
-- field and cache name for context when serialized.
--
-- These are created by the @cachix export@ command.
data CachixPullToken = CachixPullToken
 { cacheName :: Text
 , secretToken :: Text
 } deriving (Eq, Ord)

instance ToJSON CachixPullToken where
  toJSON a = object
    [ "kind" .= String "CachixPullToken"
    , "cacheName" .= cacheName a
    , "secretToken" .= secretToken a
    ]
  toEncoding a = pairs
    (  "kind" .= String "CachixPullToken"
    <> "cacheName" .= cacheName a
    <> "secretToken" .= secretToken a
    )

instance FromJSON CachixPullToken where
  parseJSON = withKind "CachixPullToken" $ \o ->
    CachixPullToken
      <$> o .: "cacheName"
      <*> o .: "secretToken"
