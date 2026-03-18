module Cachix.DebugInfod.BinaryCache
  ( fetchDebugInfoRedirect,
    streamNar,
    fetchStorePathNar,
  )
where

import Cachix.API qualified as API
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant (cachixClient)
import Cachix.Client.URI qualified as URI
import Cachix.DebugInfod.Types (DebugInfoRedirect, DebugInfodEnv (..))
import Cachix.Types.NarInfoHash qualified as NarInfoHash
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)
import Nix.NarInfo qualified as NarInfo
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API.ResponseHeaders (getResponse)
import Servant.Auth.Client (Token (..))
import Servant.Client.Streaming (runClientM)
import System.IO.Error (userError)

-- | Maximum size for metadata responses (narinfo, JSON redirect).
-- Matches Rust SMALL_FILE_SIZE. Does not apply to NAR streams.
maxMetadataSize :: Int64
maxMetadataSize = 1024 * 1024 - 1

-- | Build a request with optional auth for the binary cache.
mkRequest :: DebugInfodEnv -> Text -> IO HTTP.Request
mkRequest env path = do
  let baseUrl = URI.toByteString (envCacheBaseUrl env)
      url = baseUrl <> "/" <> toS path
  req <- HTTP.parseRequest (toS url)
  pure $ case envAuthToken env of
    Nothing -> req
    Just (Token t) -> req {HTTP.requestHeaders = ("Authorization", "Bearer " <> toS t) : HTTP.requestHeaders req}

-- | Fetch the debug info redirect JSON for a build ID.
-- Tries without .debug suffix first, then with.
fetchDebugInfoRedirect :: DebugInfodEnv -> Text -> IO (Maybe DebugInfoRedirect)
fetchDebugInfoRedirect env buildId = do
  result1 <- tryFetch ("debuginfo/" <> buildId)
  case result1 of
    Just r -> pure (Just r)
    Nothing -> tryFetch ("debuginfo/" <> buildId <> ".debug")
  where
    tryFetch :: Text -> IO (Maybe DebugInfoRedirect)
    tryFetch path = do
      req <- mkRequest env path
      resp <- HTTP.httpLbs req (envHttpManager env)
      let status = statusCode (HTTP.responseStatus resp)
          body = HTTP.responseBody resp
      if status == 200
        then do
          guardResponseSize body path
          case Aeson.eitherDecode body of
            Right redirect -> pure (Just redirect)
            Left _ -> pure Nothing
        else pure Nothing

-- | Fetch a resource from the binary cache. Returns the response body on 200.
fetchFromCache :: DebugInfodEnv -> Text -> IO (Maybe LBS.ByteString)
fetchFromCache env path = do
  req <- mkRequest env path
  resp <- HTTP.httpLbs req (envHttpManager env)
  let status = statusCode (HTTP.responseStatus resp)
  if status == 200
    then pure (Just (HTTP.responseBody resp))
    else pure Nothing

-- | Stream a NAR from the binary cache, returning the response body as lazy ByteString.
streamNar :: DebugInfodEnv -> Text -> IO (Maybe LBS.ByteString)
streamNar env narPath = fetchFromCache env ("debuginfo/" <> narPath)

-- | Fetch the NAR for a store path by first fetching its narinfo
-- via the typed Servant API, then fetching the actual NAR binary.
-- Returns (narUrl, narData) on success.
fetchStorePathNar :: DebugInfodEnv -> Text -> IO (Maybe (Text, LBS.ByteString))
fetchStorePathNar env storeHash = do
  let token = fromMaybe (Token "") (envAuthToken env)
      narInfoHash = NarInfoHash.NarInfoHash storeHash
  result <-
    retryHttp $
      (`runClientM` envClientEnv env) $
        API.narinfo
          cachixClient
          token
          (envCacheName env)
          narInfoHash
  case result of
    Left _ -> pure Nothing
    Right headersNarInfo -> do
      let narUrl = NarInfo.url (getResponse headersNarInfo)
      narData <- fetchFromCache env narUrl
      pure $ (narUrl,) <$> narData

-- | Throw if a metadata response exceeds the size limit.
guardResponseSize :: LBS.ByteString -> Text -> IO ()
guardResponseSize body path =
  let size = LBS.length body
   in when (size > maxMetadataSize) $
        throwIO $
          userError $
            "metadata response for "
              <> toS path
              <> " exceeds size limit ("
              <> show size
              <> " bytes)"
