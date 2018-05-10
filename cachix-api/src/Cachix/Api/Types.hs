{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Cachix.Api.Types where

import Data.Aeson           (FromJSON, ToJSON)
import Data.ByteString      (ByteString)
import Data.Monoid          ((<>))
import Data.Text            (Text, takeEnd, dropEnd)
import GHC.Generics         (Generic)
import Servant.API


data NixCacheInfo = NixCacheInfo
  { storeDir :: Text
  , wantMassQuery :: Integer
  , priority :: Integer
  } deriving (Generic, Show, FromJSON, ToJSON)

-- narinfo url includes storePath hash and .narinfo suffix
data NarInfo = NarInfo
  { storePath :: Text
    -- ^ absolute path of the derivation in nix store
  , url :: Text
    -- ^ relative url (to current domain) to download nar file
  , compression :: Text
    -- ^ name of the compression algorithm, eg. xz
  , fileHash :: Text
    -- ^ sha256 hash of the compressed nar file
    -- NOTE: to compute use "nix-hash --type sha256 --flat --base32"
  , fileSize :: Integer
    -- ^ file size of compressed nar file
    -- NOTE: du -b
  , narHash :: Text
    -- ^ sha256 hash of the decompressed nar file
    -- NOTE: to compute use "nix-hash --type sha256 --flat --base32"
  , narSize :: Integer
    -- ^ file size of decompressed nar file
    -- NOTE: du -b
  , references :: [Text]
    -- ^ immediate dependencies of the storePath
    -- NOTE: nix-store -q --references
  , deriver :: Text
    -- ^ relative store path (to nix store root) of the deriver
    -- NOTE: nix-store -q --deriver
  , sig :: Text
    -- ^ signature of fields: storePath, narHash, narSize, refs
  } deriving (Generic, Show, FromJSON, ToJSON)

-- | Client create type
data NarInfoCreate = NarInfoCreate
  { cStoreHash :: Text -- ^ $storePrefix / $storeHash - $storeSuffix
  , cStoreSuffix :: Text -- ^ $storePrefix / $storeHash - $storeSuffix
  , cNarHash :: Text
  , cNarSize :: Int
  , cFileHash :: Text
  , cFileSize :: Int
  , cReferences :: [Text]
  , cDeriver :: Text
  , cSig :: Text
  } deriving (Generic, Show, FromJSON, ToJSON)

data BinaryCache = BinaryCache
  { name :: Text
  , uri :: Text
  , publicSigningKeys :: [Text]
  } deriving (Show, Generic, FromJSON, ToJSON)

data BinaryCacheCreate = BinaryCacheCreate
  { publicSigningKey :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

-- | checksum of nar.xz file
newtype NarC = NarC Text deriving Generic

-- | Store path hash
newtype NarInfoC = NarInfoC Text deriving Generic

instance FromHttpApiData NarC where
  parseUrlPiece s =
    if takeEnd 7 s == ".nar.xz"
    then Right $ NarC s
    else Left ""

instance ToHttpApiData NarC where
  toUrlPiece (NarC n) = n <> ".nar.xz"

instance FromHttpApiData NarInfoC where
  parseUrlPiece s =
    if takeEnd 8 s == ".narinfo"
    then Right $ NarInfoC (dropEnd 8 s)
    else Left ""

instance ToHttpApiData NarInfoC where
  toUrlPiece (NarInfoC n) = n <> ".narinfo"


data BinaryCacheError = BinaryCacheError
  { error :: Text
  } deriving (Generic, FromJSON, ToJSON)

data User = User
  { fullname :: Text
  , username :: Text
  } deriving (Generic, FromJSON, ToJSON)
