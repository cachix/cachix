{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Api (
  api,
  servantApi,
  swaggerDoc,
  BinaryCache(..),
  API,
  NixCacheInfo(..),
  NarInfoC(..),
  NarC(..),
  NarInfo(..),
  Nar(..)
  ) where

import Control.Lens
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy(..))
import Data.Swagger
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Generic
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI

import Cachix.Types.ContentTypes (XNixCacheInfo, XNixNarInfo, XNixNar)


newtype Nar = Nar
  { unnar :: ByteString
  } deriving (Generic, Show)

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
    then Right $ NarC (dropEnd 7 s)
    else Left ""

instance FromHttpApiData NarInfoC where
  parseUrlPiece s =
    if takeEnd 8 s == ".narinfo"
    then Right $ NarInfoC (dropEnd 8 s)
    else Left ""

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

-- implement URLs for getFile function in Nix binary cache
data BinaryCache route = BinaryCache
  { -- https://cache.nixos.org/nix-cache-info
    nixCacheInfo :: route :-
      "nix-cache-info" :> Get '[XNixCacheInfo] NixCacheInfo
  -- Hydra: src/lib/Hydra/View/NixNAR.pm
  , nar :: route :-
      "nar" :> Capture "nar" NarC :> Get '[XNixNar] Nar
  -- Hydra: src/lib/Hydra/View/NarInfo.pm
  , narinfo :: route :-
      Capture "narinfo" NarInfoC :> Get '[XNixNarInfo] NarInfo
  } deriving Generic
  -- TODO: log files
  -- TODO: nar.ls json file

type ServantAPI = ToServant (BinaryCache AsApi)
type API = ServantAPI :<|> SwaggerSchemaUI "docs" "swagger.json"

servantApi :: Proxy ServantAPI
servantApi = Proxy

api :: Proxy API
api = Proxy

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy ServantAPI)
    & info.title       .~ "cachix.org API"
    & info.version     .~ "1.0"
    & info.description ?~ "TODO"
