{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
module Cachix.Client
  ( main
  , go
  --, client
  ) where

import Data.String.Here
import Protolude
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified URI.ByteString as UBS
import URI.ByteString hiding (Scheme)
import URI.ByteString.QQ
import Servant.API hiding (BaseUrl, URI)
import Servant.Client

import Cachix.Api (api)
import Cachix.Client.OptionsParser (CachixCommand(..), getOpts)
import Cachix.Client.Config        (Config(..), readConfig, writeConfig, mkConfig)

-- | TODO: move to Internal module
getBaseUrl :: URIRef Absolute -> BaseUrl
getBaseUrl uri@URI{..} =
  case uriAuthority of
    Nothing -> panic "missing host in url"
    Just authority ->
      BaseUrl
        getScheme
        (toS (hostBS (authorityHost authority)))
        getPort
        (toS uriPath)
      where
        getScheme :: Scheme
        getScheme = case uriScheme of
          UBS.Scheme "http" -> Http
          UBS.Scheme "https" -> Https
          _ -> panic "uri can only be http/https"

        getPort :: Int
        getPort = maybe defaultPort portNumber $ authorityPort authority

        defaultPort :: Int
        defaultPort = case getScheme of
          Http -> 80
          Https -> 443

main :: IO ()
main = do
  opts <- getOpts
  config <- readConfig
  go () config opts

go :: CachixClient -> Maybe Config -> CachixCommand -> IO ()
go cclient maybeConfig (AuthToken token) = do
  -- TODO: check that token actually authenticates!
  writeConfig $ case maybeConfig of
    Just config -> config { authToken = token }
    Nothing -> mkConfig token
  putStrLn authTokenNext
go cclient config (Create name) = undefined
go cclient config (Sync maybeName) = undefined
go cclient config (Use name) = undefined

authTokenNext :: Text
authTokenNext = [hereLit|
Continue by creating a binary cache with:

    $ cachix create <name>
|]
type CachixClient = ()

{-
getAllBooks :: ClientM Text
postNewBook :: ClientM Text
(getAllBooks :<|> postNewBook) = client api

env :: IO ()
env = do
  manager <- newManager defaultManagerSettings
  let env = mkClientEnv manager $ getBaseUrl [uri|http://localhost:8081|]
  res <- runClientM getAllBooks env
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right books -> print books
-}
