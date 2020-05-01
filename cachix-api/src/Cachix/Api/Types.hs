module Cachix.Api.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (dropEnd, takeEnd)
import Protolude
import Servant.API

data NixCacheInfo
  = NixCacheInfo
      { storeDir :: Text,
        wantMassQuery :: Integer,
        priority :: Integer
      }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data BinaryCache
  = BinaryCache
      { name :: Text,
        uri :: Text,
        isPublic :: Bool,
        publicSigningKeys :: [Text],
        githubUsername :: Text
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
        activeSubscription :: SubscriptionType,
        subscriptionAccountId :: Maybe Text
      }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data SubscriptionType = Community | Starter | Basic | Pro
  deriving (Generic, FromJSON, ToJSON, ToSchema, Show, Read)
