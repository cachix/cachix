module Cachix.Api.Types where

import Cachix.Types.Permission (Permission)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (dropEnd, takeEnd)
import Data.Time.Clock (UTCTime)
import Protolude
import Servant.API

data NixCacheInfo
  = NixCacheInfo
      { storeDir :: Text,
        wantMassQuery :: Integer,
        priority :: Integer
      }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- narinfo url includes storePath hash and .narinfo suffix
data NarInfo
  = NarInfo
      { -- | absolute path of the derivation in nix store
        storePath :: Text,
        -- | relative url (to current domain) to download nar file
        url :: Text,
        -- | name of the compression algorithm, eg. xz
        compression :: Text,
        -- | sha256 hash of the compressed nar file
        -- NOTE: to compute use "nix-hash --type sha256 --flat"
        fileHash :: Text,
        -- | file size of compressed nar file
        -- NOTE: du -b
        fileSize :: Integer,
        -- | sha256 hash of the decompressed nar file
        -- NOTE: to compute use "nix-hash --type sha256 --flat --base32"
        narHash :: Text,
        -- | file size of decompressed nar file
        -- NOTE: du -b
        narSize :: Integer,
        -- | immediate dependencies of the storePath
        -- NOTE: nix-store -q --references
        references :: [Text],
        -- | relative store path (to nix store root) of the deriver
        -- NOTE: nix-store -q --deriver
        deriver :: Text,
        -- | signature of fields: storePath, narHash, narSize, refs
        sig :: Text
      }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data BinaryCache
  = BinaryCache
      { name :: Text,
        uri :: Text,
        isPublic :: Bool,
        publicSigningKeys :: [Text],
        githubUsername :: Text,
        permission :: Permission
      }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema, NFData)

newtype BinaryCacheError
  = BinaryCacheError
      { error :: Text
      }
  deriving (Generic, FromJSON, ToJSON)

-- | Store path hash
newtype NarInfoC = NarInfoC Text deriving (Generic, ToSchema, ToParamSchema)

instance FromHttpApiData NarInfoC where
  parseUrlPiece s =
    if takeEnd 8 s == ".narinfo"
      then Right $ NarInfoC (dropEnd 8 s)
      else Left ""

instance ToHttpApiData NarInfoC where
  toUrlPiece (NarInfoC n) = n <> ".narinfo"

data User
  = User
      { fullname :: Maybe Text,
        username :: Text,
        email :: Maybe Text,
        hasOrgsAcccess :: Bool,
        isLegacyPaymentProvider :: Bool,
        subscriptionTrialStartedOn :: Maybe UTCTime,
        subscriptionPlan :: SubscriptionPlan,
        subscriptionStatus :: SubscriptionStatus,
        subscriptionPlanId :: Maybe Integer,
        subscriptionAccountId :: Maybe Text,
        subscriptionUpdateURL :: Maybe Text,
        subscriptionCancelURL :: Maybe Text
      }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data SubscriptionPlan = Community | Starter | Basic | Pro
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show, Read, Eq)

data SubscriptionStatus = NoSubscription | Trial | Subscribed | Cancelled
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show, Read, Eq)
