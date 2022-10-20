{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.URI
  ( URI,
    fromURIRef,
    getScheme,
    getHostname,
    appendSubdomain,
    getPortFor,
    getPath,
    requiresSSL,
    parseURI,
    serialize,
    getBaseUrl,
    defaultCachixURI,
    defaultCachixBaseUrl,
    UBS.Host (..),
    UBS.Scheme (..),
    UBS.Port (..),
  )
where

import Control.Monad (fail)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Either.Validation (Validation (Failure, Success))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Dhall
import qualified Dhall.Core
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Client
import qualified URI.ByteString as UBS
import qualified URI.ByteString.QQ as UBS

-- Default URIs

defaultCachixURI :: URI
defaultCachixURI = fromURIRef [UBS.uri|https://cachix.org|]

defaultCachixBaseUrl :: BaseUrl
defaultCachixBaseUrl = getBaseUrl defaultCachixURI

newtype URI = URI {getUri :: UBS.URIRef UBS.Absolute}
  deriving stock (Eq, Show)

fromURIRef :: UBS.URIRef UBS.Absolute -> URI
fromURIRef = URI

getScheme :: URI -> UBS.Scheme
getScheme = UBS.uriScheme . getUri

getHostname :: URI -> UBS.Host
getHostname = UBS.authorityHost . fromJust . UBS.uriAuthority . getUri

-- TODO: lenses?
appendSubdomain :: Text -> URI -> URI
appendSubdomain domain uri =
  let UBS.URI uScheme uAuthority uPath uQuery uFragment = getUri uri
      UBS.Authority aUserInfo aHost aPort = fromJust uAuthority
      newHost = UBS.Host $ toS domain <> "." <> UBS.hostBS aHost
   in URI $
        UBS.URI
          uScheme
          (Just (UBS.Authority aUserInfo newHost aPort))
          uPath
          uQuery
          uFragment

getPortFor :: UBS.Scheme -> Maybe UBS.Port
getPortFor scheme = Map.lookup scheme UBS.httpDefaultPorts

getPath :: URI -> ByteString
getPath = UBS.uriPath . getUri

requiresSSL :: UBS.Scheme -> Bool
requiresSSL (UBS.Scheme "https") = True
requiresSSL _ = False

parseURI :: ByteString -> Either UBS.URIParseError URI
parseURI bs = fromURIRef <$> UBS.parseURI UBS.strictURIParserOptions bs

serialize :: StringConv BS.ByteString s => URI -> s
serialize = toS . UBS.serializeURIRef' . getUri

instance Aeson.ToJSON URI where
  toJSON (URI uri) = Aeson.String . toS . UBS.serializeURIRef' $ uri

instance Aeson.FromJSON URI where
  parseJSON = Aeson.withText "URI" $ \text ->
    either (fail . show) (return . URI) $
      UBS.parseURI UBS.strictURIParserOptions (toS text)

instance Dhall.FromDhall URI where
  autoWith opts =
    Dhall.Decoder extract expected
    where
      textDecoder :: Dhall.Decoder Text
      textDecoder = Dhall.autoWith opts

      extract expression =
        case Dhall.extract textDecoder expression of
          Success x -> case UBS.parseURI UBS.strictURIParserOptions (toS x) of
            Left exception -> Dhall.extractError (show exception)
            Right path -> Success (fromURIRef path)
          Failure e -> Failure e

      expected = Dhall.expected textDecoder

instance Dhall.ToDhall URI where
  injectWith opts = Dhall.Encoder embed declared
    where
      textEncoder :: Dhall.Encoder Text
      textEncoder = Dhall.injectWith opts

      embed (URI uri) = Dhall.embed textEncoder $ toS (UBS.serializeURIRef' uri)

      declared = Dhall.Core.Text

-- | Partial function from URI to BaseUrl
--
-- TODO: We should error out during the parsing stage with a nice error.
-- TODO: make getBaseUrl internal
getBaseUrl :: URI -> BaseUrl
getBaseUrl (URI uriref) =
  case UBS.uriAuthority uriref of
    Nothing -> panic "missing host in url"
    Just authority ->
      BaseUrl scheme hostname port path
      where
        scheme :: Scheme
        scheme = case UBS.uriScheme uriref of
          UBS.Scheme "http" -> Http
          UBS.Scheme "https" -> Https
          _ -> panic "uri can only be http/https"

        hostname = toS $ UBS.hostBS (UBS.authorityHost authority)

        port :: Int
        port = maybe defaultPort UBS.portNumber $ UBS.authorityPort authority

        defaultPort :: Int
        defaultPort = case scheme of
          Http -> 80
          Https -> 443

        path = toS $ removeTrailingSlash (UBS.uriPath uriref)

        -- Servant expects the trailing slash to be removed
        -- https://hackage.haskell.org/package/servant-client-core-0.19/docs/Servant-Client-Core.html#v:parseBaseUrl
        removeTrailingSlash :: ByteString -> ByteString
        removeTrailingSlash "" = ""
        removeTrailingSlash str = case Char8.last str of
          '/' -> Char8.init str
          _ -> str
