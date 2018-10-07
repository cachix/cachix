module Cachix.Api.Types where

import Data.Aeson           (FromJSON, ToJSON)
import Data.Monoid          ((<>))
import Data.Swagger         (ToSchema, ToParamSchema)
import Data.Text            (Text, takeEnd, dropEnd)
import GHC.Generics         (Generic)
import Servant.API


data NixCacheInfo = NixCacheInfo
  { storeDir :: Text
  , wantMassQuery :: Integer
  , priority :: Integer
  } deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

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
    -- NOTE: to compute use "nix-hash --type sha256 --flat"
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
  } deriving (Generic, Show, FromJSON, ToJSON, ToSchema)


data BinaryCache = BinaryCache
  { name :: Text
  , uri :: Text
  , isPublic :: Bool
  , publicSigningKeys :: [Text]
  , githubUsername :: Text
  } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype BinaryCacheError = BinaryCacheError
  { error :: Text
  } deriving (Generic, FromJSON, ToJSON)

-- | Hash of nar.xz file
newtype NarC = NarC Text deriving (Generic, ToSchema, ToParamSchema)

instance FromHttpApiData NarC where
  parseUrlPiece s =
    if takeEnd 7 s == ".nar.xz"
    then Right $ NarC (dropEnd 7 s)
    else Left ""

instance ToHttpApiData NarC where
  toUrlPiece (NarC n) = n <> ".nar.xz"

-- | Store path hash
newtype NarInfoC = NarInfoC Text deriving (Generic, ToSchema, ToParamSchema)

instance FromHttpApiData NarInfoC where
  parseUrlPiece s =
    if takeEnd 8 s == ".narinfo"
    then Right $ NarInfoC (dropEnd 8 s)
    else Left ""

instance ToHttpApiData NarInfoC where
  toUrlPiece (NarInfoC n) = n <> ".narinfo"

data User = User
  { fullname :: Text
  , username :: Text
  , email :: Maybe Text
  } deriving (Generic, FromJSON, ToJSON, ToSchema)
