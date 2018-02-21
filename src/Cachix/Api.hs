{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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


newtype NixCacheInfo = NixCacheInfo ByteString
instance ToSchema NixCacheInfo where
  declareNamedSchema _ = return $ NamedSchema (Just "NixCacheInfo") binarySchema

data CacheAPIG route = CacheAPI
  { -- https://cache.nixos.org/nix-cache-info
    nixCacheInfo :: route :-
      "nix-cache-info" :> Get '[OctetStream] NixCacheInfo
  , nar :: route :-
      Capture "x" Text :> Get '[JSON] Text
  , narinfo :: route :-
      Capture "x" Text :> Get '[JSON] Text
  } deriving Generic

type CacheAPI = ToServant (CacheAPIG AsApi)

api :: Proxy CacheAPI
api = Proxy

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy CacheAPI)
    & info.title       .~ "cachix.org API"
    & info.version     .~ "1.0"
    & info.description ?~ "TODO"
