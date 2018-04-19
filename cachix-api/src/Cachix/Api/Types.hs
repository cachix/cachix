{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Cachix.Api.Types where

import Data.Aeson           (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Swagger
import Data.Text
import GHC.Generics         (Generic)
import Servant.API
import Web.Cookie           (SetCookie)


newtype Nar = Nar
  { unnar :: ByteString
  } deriving (Generic, Show)

data NixCacheInfo = NixCacheInfo
  { storeDir :: Text
  , wantMassQuery :: Integer
  , priority :: Integer
  } deriving (Generic, Show)

data NarInfo = NarInfo
  { storePath :: Text
  , url :: Text
  , compression :: Text
  , fileHash :: Text
  , fileSize :: Integer
  , narHash :: Text
  , narSize :: Integer
  , references :: [Text]
  , deriver :: Text
  , sig :: Text
  } deriving (Generic, Show)


data BinaryCache = BinaryCache
  { name :: Text
  , publicSigningKey :: Text
  } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)


instance ToSchema Nar where
  -- TODO: properly format the field
  declareNamedSchema _ = return $ NamedSchema (Just "Nar") binarySchema
instance ToSchema NixCacheInfo
instance ToSchema NarInfo

newtype NarC = NarC Text deriving Generic
newtype NarInfoC = NarInfoC Text deriving Generic

instance ToParamSchema NarC
instance ToParamSchema NarInfoC

instance FromHttpApiData NarC where
  parseUrlPiece s =
    if takeEnd 7 s == ".nar.xz"
    then Right $ NarC s
    else Left ""

instance FromHttpApiData NarInfoC where
  parseUrlPiece s =
    if takeEnd 8 s == ".narinfo"
    then Right $ NarInfoC (dropEnd 8 s)
    else Left ""

-- TODO: https://github.com/haskell-servant/servant-auth/pull/42#issuecomment-381279499
instance ToParamSchema SetCookie where
  toParamSchema _ = mempty -- TODO: cookie instances for swagger
