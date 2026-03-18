{-# LANGUAGE DataKinds #-}

module Cachix.DebugInfod.Server
  ( runServer,
  )
where

import Cachix.DebugInfod.BinaryCache qualified as BinaryCache
import Cachix.DebugInfod.BuildId
  ( BuildId,
    buildIdText,
    buildIdToDebugPath,
    buildIdToExecutablePath,
    buildIdToSourceOverlayPath,
    buildIdToSourcePath,
  )
import Cachix.DebugInfod.Cache qualified as Cache
import Cachix.DebugInfod.Nar qualified as Nar
import Cachix.DebugInfod.SourceSelection qualified as SourceSelection
import Cachix.DebugInfod.StorePath qualified as StorePath
import Cachix.DebugInfod.Types (DebugInfodEnv (..))
import Cachix.DebugInfod.Vfs qualified as Vfs
import Cachix.Types.DebugInfo qualified as DebugInfo
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.HTTP.Types.Status (status200, status404, status500)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Protolude hiding (Handler, toS)
import Protolude.Conv
import Servant
import System.FilePath ((</>))

type DebugInfodAPI =
  "buildid"
    :> Capture "buildid" BuildId
    :> ( "debuginfo" :> Raw
           :<|> "executable" :> Raw
           :<|> "source" :> CaptureAll "path" Text :> Raw
       )

-- | Start the debuginfod HTTP server.
runServer :: DebugInfodEnv -> Int -> IO ()
runServer env port = do
  Cache.spawnCleanupThread (envFetcherCache env)
  putText $ "debuginfod server listening on port " <> show port
  Warp.run port (serve (Proxy @DebugInfodAPI) (server env))

server :: DebugInfodEnv -> Server DebugInfodAPI
server env buildId =
  serveFileApp (fetchDebugInfo env buildId)
    :<|> serveFileApp (fetchExecutable env buildId)
    :<|> serveSourceApp env buildId

serveFileApp :: IO (Maybe FilePath) -> Tagged Handler Application
serveFileApp action = Tagged $ \_req respond -> do
  result <- try @SomeException action
  let headers = [("Content-Type", "application/octet-stream")]
  case result of
    Left err ->
      respond $ Wai.responseLBS status500 headers (LBS.fromStrict (toS ("internal error: " <> show err :: Text)))
    Right Nothing ->
      respond $ Wai.responseLBS status404 headers "not found in cache"
    Right (Just fp) ->
      respond $ Wai.responseFile status200 headers fp Nothing

serveSourceApp :: DebugInfodEnv -> BuildId -> [Text] -> Tagged Handler Application
serveSourceApp env buildId pathParts =
  serveFileApp $
    fetchSource env buildId (toS (T.intercalate "/" pathParts))

-- | Fetch debug info for a build ID, using the concurrent cache.
fetchDebugInfo :: DebugInfodEnv -> BuildId -> IO (Maybe FilePath)
fetchDebugInfo env buildId = do
  entryDir <- fetchDebugOutput env buildId
  case entryDir of
    Nothing -> pure Nothing
    Just dir -> do
      let rp = Vfs.RestrictedPath dir (dir </> buildIdToDebugPath buildId)
      resolved <- Vfs.resolveInsideRoot rp
      pure (Vfs.resolvedPath <$> resolved)

-- | Fetch the executable for a build ID.
-- Reads the .executable symlink target, parses it as a store path,
-- and fetches the file via serveNarContent.
fetchExecutable :: DebugInfodEnv -> BuildId -> IO (Maybe FilePath)
fetchExecutable env buildId = runMaybeT $ do
  dir <- MaybeT $ fetchDebugOutput env buildId
  let execSymlink = dir </> buildIdToExecutablePath buildId
  target <- MaybeT $ Vfs.readSymlinkSafe execSymlink
  sp <- eitherToMaybeT $ StorePath.parseStorePath (toS target)
  content <- MaybeT $ BinaryCache.fetchStoreFile env sp
  let cachePath = dir </> "executable"
  liftIO $ LBS.writeFile cachePath content
  pure cachePath

-- | Fetch a source file for a build ID.
-- Two modes:
--   1. If the path starts with "nix/store/", fetch the store path directly
--   2. Otherwise, look at the .source symlink in the debug output and
--      use fuzzy matching to find the file
fetchSource :: DebugInfodEnv -> BuildId -> FilePath -> IO (Maybe FilePath)
fetchSource env buildId requestPath
  | "nix/store/" `isPrefixOf` requestPath = fetchSourceFromStorePath env requestPath
  | otherwise = fetchSourceFromDebugOutput env buildId requestPath

-- | Fetch source directly from a store path via serveNarContent.
-- Handles GCC hash demangling (uppercased hashes in debug symbols).
fetchSourceFromStorePath :: DebugInfodEnv -> FilePath -> IO (Maybe FilePath)
fetchSourceFromStorePath env requestPath = runMaybeT $ do
  let absolute = "/" <> requestPath
      demangled = StorePath.demangle (toS absolute)
  sp <- eitherToMaybeT $ StorePath.parseStorePath demangled
  let fc = envFetcherCache env
      cacheKey = StorePath.storePathHash sp <> "-" <> T.replace "/" "-" (toS (StorePath.storePathRelative sp))
  cacheDir <- MaybeT $
    Cache.fetchOrGet fc cacheKey $ \partialDir -> do
      mContent <- BinaryCache.fetchStoreFile env sp
      case mContent of
        Nothing -> pure False
        Just content -> do
          LBS.writeFile (partialDir </> "file") content
          pure True
  pure (cacheDir </> "file")

-- | Fetch source from the debug output using the .source symlink.
-- The .source symlink points to a directory containing the source tree.
-- Downloads the full source NAR since fuzzy matching requires the full tree.
fetchSourceFromDebugOutput :: DebugInfodEnv -> BuildId -> FilePath -> IO (Maybe FilePath)
fetchSourceFromDebugOutput env buildId requestPath = runMaybeT $ do
  dir <- MaybeT $ fetchDebugOutput env buildId
  let sourceSymlink = dir </> buildIdToSourcePath buildId
  sourceTarget <- MaybeT $ Vfs.readSymlinkSafe sourceSymlink
  sourceSp <- eitherToMaybeT $ StorePath.parseStorePath (toS sourceTarget)
  sourceDir <- MaybeT $ fetchSourceNar env sourceSp
  overlayDir <- liftIO $ resolveOverlay env dir buildId sourceDir
  MaybeT $ SourceSelection.findSourceFile sourceDir overlayDir requestPath

-- | Fetch and cache a source store path NAR via narinfo + streaming.
fetchSourceNar :: DebugInfodEnv -> StorePath.StorePath -> IO (Maybe FilePath)
fetchSourceNar env sp = do
  let spHash = StorePath.storePathHash sp
      fc = envFetcherCache env
  Cache.fetchOrGet fc ("source-" <> spHash) $ \partialDir -> do
    result <- BinaryCache.withStoreNarStream env spHash $ \narUrl source ->
      Nar.unpackNar narUrl source partialDir
    pure (isJust result)

-- | Fetch and cache the debug output NAR for a build ID.
fetchDebugOutput :: DebugInfodEnv -> BuildId -> IO (Maybe FilePath)
fetchDebugOutput env buildId = do
  let bid = buildIdText buildId
      fc = envFetcherCache env
  Cache.fetchOrGet fc bid $ \partialDir -> do
    redirect <- BinaryCache.fetchDebugInfoRedirect env bid
    case redirect of
      Nothing -> pure False
      Just DebugInfo.DebugInfoRedirect {DebugInfo.redirectArchive = archive} -> do
        result <- BinaryCache.withDebugNarStream env archive $ \source ->
          Nar.unpackNar archive source partialDir
        pure (isJust result)

-- | Resolve the source overlay directory, falling back to the source directory.
resolveOverlay :: DebugInfodEnv -> FilePath -> BuildId -> FilePath -> IO FilePath
resolveOverlay env dir buildId sourceDir = do
  let overlaySymlink = dir </> buildIdToSourceOverlayPath buildId
  mOverlayTarget <- Vfs.readSymlinkSafe overlaySymlink
  case mOverlayTarget of
    Nothing -> pure sourceDir
    Just overlayTarget ->
      case StorePath.parseStorePath (toS overlayTarget) of
        Left _ -> pure sourceDir
        Right overlaySp -> do
          mOvDir <- fetchSourceNar env overlaySp
          pure (fromMaybe sourceDir mOvDir)

eitherToMaybeT :: (Monad m) => Either a b -> MaybeT m b
eitherToMaybeT = MaybeT . pure . either (const Nothing) Just
