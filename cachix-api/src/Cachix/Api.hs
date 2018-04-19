{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Api
  ( api
  , servantApi
  , swaggerDoc
  , BinaryCacheAPI(..)
  , API
  , module Cachix.Api.Types
  ) where

import Control.Lens

import Data.Proxy (Proxy(..))
import Data.Swagger hiding (Header)
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Auth
import Servant.Auth.Swagger
import Servant.Generic
import Servant.Swagger
import Servant.Swagger.UI
import Web.Cookie                (SetCookie)

import Cachix.Types.ContentTypes (XNixCacheInfo, XNixNarInfo, XNixNar)
import Cachix.Types.Servant      (Get302)
import Cachix.Types.Session      (Session)
import Cachix.Api.Types


type CachixAuth = Auth '[Cookie] Session

-- implement URLs for getFile function in Nix binary cache
data BinaryCacheAPI route = BinaryCacheAPI
  { -- https://cache.nixos.org/nix-cache-info
    nixCacheInfo :: route :-
      "nix-cache-info" :> Get '[XNixCacheInfo] NixCacheInfo
  -- Hydra: src/lib/Hydra/View/NixNAR.pm
  , nar :: route :-
      "nar" :> Capture "nar" NarC :> Get '[XNixNar] Nar
  -- Hydra: src/lib/Hydra/View/NarInfo.pm
  , narinfo :: route :-
      Capture "narinfo" NarInfoC :> Get '[XNixNarInfo] NarInfo
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
  , root :: route :-
      CachixAuth :>
      Get '[PlainText] Text
  , rootPost :: route :-
      CachixAuth :>
      ReqBody '[JSON] BinaryCache :>
      Post '[JSON] NoContent
  } deriving Generic

type ServantAPI = ToServant (BinaryCacheAPI AsApi)
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
