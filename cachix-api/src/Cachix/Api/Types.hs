{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Cachix.Api.Types where

import Data.Aeson           (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid          ((<>))
import Data.Text            (Text, takeEnd, dropEnd)
import GHC.Generics         (Generic)
import Servant.API


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
  } deriving (Generic, Show, FromJSON, ToJSON)


data BinaryCache = BinaryCache
  { publicSigningKeys :: [Text]
  } deriving (Show, Generic, FromJSON, ToJSON)

data BinaryCacheCreate = BinaryCacheCreate
  { publicSigningKey :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype NarC = NarC Text deriving Generic
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
