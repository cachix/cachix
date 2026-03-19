module Cachix.DebugInfod.BinaryCache
  ( fetchDebugInfoRedirect,
    withDebugNarStream,
    withStoreNarStream,
    fetchStoreFile,
  )
where

import Cachix.API qualified as API
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant (cachixClient)
import Cachix.Client.URI qualified as URI
import Cachix.DebugInfod.StorePath qualified as StorePath
import Cachix.DebugInfod.Types (DebugInfodEnv (..))
import Cachix.DebugInfod.Types qualified as Types
import Cachix.DebugInfod.Vfs qualified as Vfs
import Cachix.Types.ByteStringStreaming (LazyByteStringStreaming (..))
import Cachix.Types.DebugInfo qualified as DebugInfo
import Cachix.Types.NarFileName qualified as NarFileName
import Cachix.Types.NarInfoHash qualified as NarInfoHash
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Conduit (ConduitT, yield)
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)
import Nix.NarInfo qualified as NarInfo
import Protolude hiding (toS, yield)
import Protolude.Conv
import Servant.API.ResponseHeaders (getResponse)
import Servant.Auth.Client (Token (..))
import Servant.Client.Streaming (runClientM, withClientM)

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
fetchDebugInfoRedirect :: DebugInfodEnv -> Text -> IO (Maybe Types.DebugInfoRedirect)
fetchDebugInfoRedirect env buildId = do
  result1 <- tryFetch buildId
  case result1 of
    Just r -> pure (Just r)
    Nothing -> tryFetch (buildId <> ".debug")
  where
    tryFetch :: Text -> IO (Maybe Types.DebugInfoRedirect)
    tryFetch bid = do
      let token = fromMaybe (Token "") (envAuthToken env)
      result <-
        retryHttp $
          (`runClientM` envClientEnv env) $
            API.getDebugInfo cachixClient token (envCacheName env) bid
      pure $ case result of
        Left _ -> Nothing
        Right redirect ->
          Just
            Types.DebugInfoRedirect
              { Types.archive = DebugInfo.redirectArchive redirect,
                Types.member = DebugInfo.redirectMember redirect
              }

-- | Stream a NAR from the binary cache.
-- Uses the narURL API to get a presigned download URL, then streams from it.
-- The narPath is a relative URL like "nar/<hash>.nar.<ext>".
withDebugNarStream :: DebugInfodEnv -> Text -> (ConduitT () BS.ByteString IO () -> IO a) -> IO (Maybe a)
withDebugNarStream env narPath callback = do
  mUrl <- resolveNarDownloadUrl env narPath
  case mUrl of
    Nothing -> pure Nothing
    Just downloadUrl -> do
      req <- HTTP.parseRequest (toS downloadUrl)
      withHttpStream req env callback

-- | Stream a store path NAR by first looking up narinfo, then streaming the NAR.
-- Returns the NAR URL (with compression extension) alongside the callback result.
withStoreNarStream :: DebugInfodEnv -> Text -> (Text -> ConduitT () BS.ByteString IO () -> IO a) -> IO (Maybe a)
withStoreNarStream env storeHash callback = do
  let token = fromMaybe (Token "") (envAuthToken env)
      narInfoHash = NarInfoHash.NarInfoHash storeHash
  result <-
    retryHttp $
      (`runClientM` envClientEnv env) $
        API.narinfo cachixClient token (envCacheName env) narInfoHash
  case result of
    Left _ -> pure Nothing
    Right headersNarInfo -> do
      let narUrl = NarInfo.url (getResponse headersNarInfo)
      mDownloadUrl <- resolveNarDownloadUrl env narUrl
      case mDownloadUrl of
        Nothing -> pure Nothing
        Just downloadUrl -> do
          req <- HTTP.parseRequest (toS downloadUrl)
          withHttpStream req env (callback narUrl)

-- | Resolve a NAR path (e.g. "nar/<hash>.nar.zst") to a presigned download URL
-- via the narURL API endpoint.
resolveNarDownloadUrl :: DebugInfodEnv -> Text -> IO (Maybe Text)
resolveNarDownloadUrl env narPath = do
  let token = fromMaybe (Token "") (envAuthToken env)
  case parseNarFileName narPath of
    Nothing -> pure Nothing
    Just narFileName -> do
      result <-
        retryHttp $
          (`runClientM` envClientEnv env) $
            API.narURL cachixClient token (envCacheName env) narFileName
      pure $ case result of
        Left _ -> Nothing
        Right url -> Just url

-- | Parse "nar/<hash>.nar.<ext>" into a NarFileName.
parseNarFileName :: Text -> Maybe NarFileName.NarFileName
parseNarFileName path = do
  let stripped = fromMaybe path (T.stripPrefix "nar/" path)
  case T.splitOn "." stripped of
    [h, "nar", ext] -> Just (NarFileName.NarFileName h ext)
    _ -> Nothing

-- | Stream an HTTP response body as a conduit source.
withHttpStream :: HTTP.Request -> DebugInfodEnv -> (ConduitT () BS.ByteString IO () -> IO a) -> IO (Maybe a)
withHttpStream req env callback = do
  HTTP.withResponse req (envHttpManager env) $ \resp -> do
    let status = statusCode (HTTP.responseStatus resp)
    if status == 200
      then do
        let source = bodyReaderSource (HTTP.responseBody resp)
        Just <$> callback source
      else pure Nothing

-- | Convert an HTTP body reader into a conduit source.
bodyReaderSource :: HTTP.BodyReader -> ConduitT () BS.ByteString IO ()
bodyReaderSource reader = loop
  where
    loop = do
      chunk <- liftIO $ HTTP.brRead reader
      unless (BS.null chunk) $ do
        yield chunk
        loop

-- | Fetch a single file from a store path via serveNarContent.
fetchStoreFile :: DebugInfodEnv -> StorePath.StorePath -> IO (Maybe LBS.ByteString)
fetchStoreFile env sp = do
  let token = fromMaybe (Token "") (envAuthToken env)
      pathParts = map toS (Vfs.splitComponents (StorePath.storePathRelative sp))
  result <-
    retryHttp $
      withClientM
        (API.serveNarContent cachixClient token (envCacheName env) (StorePath.storePathHash sp) pathParts)
        (envClientEnv env)
        pure
  pure $ case result of
    Left _ -> Nothing
    Right headers -> Just (coerce (getResponse headers))
