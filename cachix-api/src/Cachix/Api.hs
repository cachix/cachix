{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cachix.Api
  ( api
  , servantApi
  , swaggerDoc
  , BinaryCacheAPI(..)
  , API
  , NixCacheInfo(..)
  , NarInfoC(..)
  , NarC(..)
  , NarInfo(..)
  , Nar(..)
  , BinaryCache(..)
  ) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy(..))
import Data.Swagger hiding (Header)
import Data.Text
import GHC.Generics
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

-- TODO: https://github.com/haskell-servant/servant-auth/pull/42#issuecomment-381279499
instance ToParamSchema SetCookie where
  toParamSchema _ = mempty -- TODO: cookie instances for swagger

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
    then Right $ NarC s
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
  -- TODO: log files
  -- TODO: nar.ls json file

type CachixAuth = Auth '[Cookie] Session

data BinaryCache = BinaryCache
  { name :: Text
  , publicSigningKey :: Text
  } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

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
