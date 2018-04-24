{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Api
  ( api
  , servantApi
  , swaggerDoc
  , CachixAPI(..)
  , BinaryCacheAPI(..)
  , CachixServantAPI
  , module Cachix.Api.Types
  ) where

import Control.Lens

import Data.Proxy (Proxy(..))
import Data.Swagger hiding (Header)
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Auth
import Servant.Generic
import Servant.Swagger
import Servant.Swagger.UI
import Web.Cookie                (SetCookie)

import Cachix.Types.ContentTypes (XNixCacheInfo, XNixNarInfo, XNixNar)
import Cachix.Types.Servant      (Get302)
import Cachix.Types.Session      (Session)
import Cachix.Api.Types
import Cachix.Api.Swagger        ()



type CachixAuth = Auth '[Cookie,JWT] Session

data BinaryCacheAPI route = BinaryCacheAPI
  { get :: route :-
      Get '[JSON] BinaryCache
  , create :: route :-
      CachixAuth :>
      ReqBody '[JSON] BinaryCacheCreate :>
      Post '[JSON] NoContent
  -- https://cache.nixos.org/nix-cache-info
  , nixCacheInfo :: route :-
      "nix-cache-info" :>
      Get '[XNixCacheInfo] NixCacheInfo
  -- Hydra: src/lib/Hydra/View/NixNAR.pm
  , nar :: route :-
      "nar" :>
      Capture "nar" NarC :>
      Get '[XNixNar] Nar
  -- Hydra: src/lib/Hydra/View/NarInfo.pm
  , narinfo :: route :-
      Capture "narinfo" NarInfoC :>
      Get '[XNixNarInfo] NarInfo
  } deriving Generic

data CachixAPI route = CachixAPI
   { root :: route :-
       CachixAuth :>
       Get '[PlainText] Text
   , login :: route :-
       "login" :>
       Get302 '[PlainText] '[]
   , loginCallback :: route :-
       "login" :>
       "callback" :>
       QueryParam "code" Text :>
       QueryParam "state" Text :>
       Get302 '[PlainText] '[ Header "Set-Cookie" SetCookie
                            , Header "Set-Cookie" SetCookie
                            ]
   , cache :: route :-
       "cache" :>
       Capture "name" Text :>
       ToServant (BinaryCacheAPI AsApi)
   } deriving Generic

type CachixServantAPI = "api" :> "v1" :> ToServant (CachixAPI AsApi)

servantApi :: Proxy CachixServantAPI
servantApi = Proxy

type API = CachixServantAPI
   :<|> "api" :> "v1" :> SwaggerSchemaUI "" "swagger.json"

api :: Proxy API
api = Proxy

swaggerDoc :: Swagger
swaggerDoc = toSwagger servantApi
    & info.title       .~ "cachix.org API"
    & info.version     .~ "1.0"
    & info.description ?~ "TODO"
