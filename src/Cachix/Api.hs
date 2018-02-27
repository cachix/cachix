{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Api (api, swaggerDoc) where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Proxy (Proxy(..))
import Data.Swagger
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Generic
import Servant.Swagger (toSwagger)

import Cachix.Types.ContentTypes (XNixCacheInfo, XNixNarInfo, XNixNar)


newtype Nar = Nar ByteString

instance ToSchema Nar where
  -- TODO: properly format the field
  declareNamedSchema _ = return $ NamedSchema (Just "Nar") binarySchema
instance ToSchema NixCacheInfo
instance ToSchema NarInfo

newtype NarC = NarC Text deriving Generic
newtype NarInfoC = NarInfoC Text deriving Generic

instance ToParamSchema NarC
instance ToParamSchema NarInfoC

data NixCacheInfo = NixCacheInfo
  { storeDir :: Text
  , wantMassQuery :: Integer
  , priority :: Integer
  } deriving Generic

data NarInfo = NarInfo
  { url :: Text
  , storePath :: Text
  , compression :: Text
  , fileHash :: Text
  , fileSize :: Integer
  , narHash :: Text
  , narSize :: Integer
  , references :: [Text]
  , deriver :: Text
  , sig :: Text
  } deriving Generic

-- implement URLs for getFile function in Nix binary cache
data BinaryCache route = BinaryCache
  { -- https://cache.nixos.org/nix-cache-info
    nixCacheInfo :: route :-
      "nix-cache-info" :> Get '[XNixCacheInfo] NixCacheInfo
  -- Hydra: src/lib/Hydra/View/NixNAR.pm
  , nar :: route :-
      Capture "nar" NarC :> Get '[XNixNar] Nar
      -- TODO: http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#the-fromhttpapidata-tohttpapidata-classes
  -- Hydra: src/lib/Hydra/View/NarInfo.pm
  , narinfo :: route :-
      Capture "narinfo" NarInfoC :> Get '[XNixNarInfo] NarInfo
  } deriving Generic
  -- TODO: log files
  -- TODO: nar.ls json file

type API = ToServant (BinaryCache AsApi)

api :: Proxy API
api = Proxy

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy API)
    & info.title       .~ "cachix.org API"
    & info.version     .~ "1.0"
    & info.description ?~ "TODO"
