{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.URI
  ( getBaseUrl,
    defaultCachixURI,
    defaultCachixBaseUrl
    )
where

import Protolude
import Servant.Client
import qualified URI.ByteString as UBS
import URI.ByteString hiding (Scheme)
import URI.ByteString.QQ

-- TODO: make getBaseUrl internal

-- | Partial function from URI to BaseUrl
--
getBaseUrl :: URIRef Absolute -> BaseUrl
getBaseUrl uriref =
  case uriAuthority uriref of
    Nothing -> panic "missing host in url"
    Just authority ->
      BaseUrl
        getScheme
        (toS (hostBS (authorityHost authority)))
        getPort
        (toS (uriPath uriref))
      where
        getScheme :: Scheme
        getScheme = case uriScheme uriref of
          UBS.Scheme "http" -> Http
          UBS.Scheme "https" -> Https
          _ -> panic "uri can only be http/https"
        getPort :: Int
        getPort = maybe defaultPort portNumber $ authorityPort authority
        defaultPort :: Int
        defaultPort = case getScheme of
          Http -> 80
          Https -> 443

defaultCachixURI :: URIRef Absolute
defaultCachixURI = [uri|https://cachix.org|]

defaultCachixBaseUrl :: BaseUrl
defaultCachixBaseUrl = getBaseUrl defaultCachixURI
