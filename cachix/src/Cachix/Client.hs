{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
module Cachix.Client
  ( main
  ) where

import Protolude
import Network.HTTP.Client         (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Servant.Auth
import Servant.Auth.Client
import URI.ByteString.QQ
import Web.Cookie (SetCookie)

import qualified Cachix.Api                  as Api
import Cachix.Client.OptionsParser (CachixCommand(..), getOpts)
import Cachix.Client.Config        (readConfig)
import Cachix.Client.Commands      as Commands
import Cachix.Client.URI           (getBaseUrl)
import Cachix.Client.Servant       (AsClient)


main :: IO ()
main = do
  opts <- getOpts
  config <- readConfig
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager $ getBaseUrl [uri|http://localhost:8081|] -- TODO: global cli arg
  -- return $ runClientM (login cachixClient) env
  case opts of -- TODO: we might want readert here with client, config and env
    AuthToken token -> Commands.authtoken env config token
    Create name -> Commands.create env config name
    Sync maybeName -> Commands.sync env config maybeName
    Use name -> Commands.use env config name


root :: Token
     -> ClientM Text
login :: ClientM (Headers '[Header "Location" Text] NoContent)
loginCallback :: Maybe Text
              -> Maybe Text
              -> ClientM (Headers
                           '[Header "Location" Text,
                             Header "Set-Cookie" SetCookie,
                             Header "Set-Cookie" SetCookie]
                           NoContent)
type Slash = ClientM Api.BinaryCache
type SlashPost = Token -> Api.BinaryCache -> ClientM NoContent
type NixCacheInfo = ClientM Api.NixCacheInfo
type Nar = Api.NarC -> ClientM Api.Nar
type NarInfo = Api.NarInfoC -> ClientM Api.NarInfo
bcApi :: Text -> (Slash :<|> SlashPost) :<|> NixCacheInfo :<|> Nar :<|> NarInfo
(root :<|> login) :<|> loginCallback :<|> bcApi = client Api.servantApi

{-
TODO: once servant-generic handles subrecords
cachixClient :: CachixAPI AsClient
cachixClient = client servantApi
-}
