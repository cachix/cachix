module Cachix.Formats.CachixPublicKey where

import           Prelude
import           Data.Text                      ( Text )
import           Data.Aeson
import           Cachix.Formats.Common

-- | An object representing a token for pulling from a cache, including "kind"
-- field and cache name for context when serialized.
--
-- These are created by the @cachix export@ command.
--
-- The public key of a cache does not serve a role in the confidentiality of
-- private caches. Confidentiality of private caches is guarded by the
-- 'Cachix.Formats.CachixPullToken.CachixPullToken'. A correct public key is
-- required for checking the /integrity/ of NARs though.
data CachixPublicKey = CachixPublicKey
 { cacheName :: Text
 , publicKey :: Text
 } deriving (Eq, Ord)

instance ToJSON CachixPublicKey where
  toJSON a = object
    [ "kind" .= String "CachixPublicKey"
    , "cacheName" .= cacheName a
    , "publicKey" .= publicKey a
    ]
  toEncoding a = pairs
    (  "kind" .= String "CachixPublicKey"
    <> "cacheName" .= cacheName a
    <> "publicKey" .= publicKey a
    )

instance FromJSON CachixPublicKey where
  parseJSON = withKind "CachixPublicKey" $ \o ->
    CachixPublicKey
      <$> o .: "cacheName"
      <*> o .: "publicKey"
