-- | Ugly glue between URI and BaseUrl
-- | TODO: mark as Internal module
{-# LANGUAGE GADTs #-}
module Cachix.Client.URI
  ( getBaseUrl
  ) where

import Protolude
import qualified URI.ByteString as UBS
import URI.ByteString hiding (Scheme)
import Servant.Client


getBaseUrl :: URIRef Absolute -> BaseUrl
getBaseUrl URI{..} =
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
