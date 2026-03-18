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
import Cachix.DebugInfod.Types (DebugInfoRedirect (..), DebugInfodEnv (..))
import Cachix.DebugInfod.Vfs qualified as Vfs
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Network.Wai.Handler.Warp qualified as Warp
import Protolude hiding (toS)
import Protolude.Conv
import Servant
import System.FilePath ((</>))

type DebugInfodAPI =
  "buildid"
    :> Capture "buildid" BuildId
    :> ( "debuginfo" :> Get '[OctetStream] LBS.ByteString
           :<|> "executable" :> Get '[OctetStream] LBS.ByteString
           :<|> "source" :> CaptureAll "path" Text :> Get '[OctetStream] LBS.ByteString
       )

-- | Start the debuginfod HTTP server.
runServer :: DebugInfodEnv -> Int -> IO ()
runServer env port = do
  Cache.spawnCleanupThread (envFetcherCache env)
  Cache.spawnCleanupThread (envStoreCache env)
  putText $ "debuginfod server listening on port " <> show port
  Warp.run port (serve (Proxy @DebugInfodAPI) (server env))

server :: DebugInfodEnv -> Server DebugInfodAPI
server env buildId =
  handleDebugInfo env buildId
    :<|> handleExecutable env buildId
    :<|> handleSource env buildId

handleDebugInfo :: DebugInfodEnv -> BuildId -> Handler LBS.ByteString
handleDebugInfo env buildId =
  serveFileOrNotFound $ fetchDebugInfo env buildId

handleExecutable :: DebugInfodEnv -> BuildId -> Handler LBS.ByteString
handleExecutable env buildId =
  serveFileOrNotFound $ fetchExecutable env buildId

handleSource :: DebugInfodEnv -> BuildId -> [Text] -> Handler LBS.ByteString
handleSource env buildId pathParts =
  serveFileOrNotFound $ fetchSource env buildId (toS (T.intercalate "/" pathParts))

serveFileOrNotFound :: IO (Maybe FilePath) -> Handler LBS.ByteString
serveFileOrNotFound action = do
  result <- liftIO $ try @SomeException action
  case result of
    Left err ->
      throwError err500 {errBody = LBS.fromStrict (toS ("internal error: " <> show err :: Text))}
    Right Nothing ->
      throwError err404 {errBody = "not found in cache"}
    Right (Just filePath) ->
      liftIO $ LBS.readFile filePath

-- | Fetch debug info for a build ID, using the concurrent cache.
-- Uses resolveInsideRoot since debug files should not follow store path symlinks.
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
-- Uses full resolve since the .executable symlink points to a store path.
fetchExecutable :: DebugInfodEnv -> BuildId -> IO (Maybe FilePath)
fetchExecutable env buildId = do
  entryDir <- fetchDebugOutput env buildId
  case entryDir of
    Nothing -> pure Nothing
    Just dir -> do
      let rp = Vfs.RestrictedPath dir (dir </> buildIdToExecutablePath buildId)
      resolved <- Vfs.resolve rp (storePathResolver env)
      pure (Vfs.resolvedPath <$> resolved)

-- | Fetch a source file for a build ID.
-- Two modes:
--   1. If the path starts with "nix/store/", fetch the store path directly
--      (with hash demangling for GCC uppercased hashes)
--   2. Otherwise, look at the .source symlink in the debug output and
--      use fuzzy matching to find the file
fetchSource :: DebugInfodEnv -> BuildId -> FilePath -> IO (Maybe FilePath)
fetchSource env buildId requestPath
  | "nix/store/" `isPrefixOf` requestPath = fetchSourceFromStorePath env requestPath
  | otherwise = fetchSourceFromDebugOutput env buildId requestPath

-- | Fetch source directly from a store path.
-- Handles GCC hash demangling (uppercased hashes in debug symbols).
fetchSourceFromStorePath :: DebugInfodEnv -> FilePath -> IO (Maybe FilePath)
fetchSourceFromStorePath env requestPath = do
  let absolute = "/" <> requestPath
      demangled = StorePath.demangle (toS absolute)
  case StorePath.parseStorePath demangled of
    Left _ -> pure Nothing
    Right sp -> do
      storeDir <- fetchStorePathOutput env sp
      case storeDir of
        Nothing -> pure Nothing
        Just sdir -> do
          let rel = StorePath.storePathRelative sp
              rp = Vfs.RestrictedPath sdir (if null rel then sdir else sdir </> rel)
          resolved <- Vfs.resolveInsideRoot rp
          pure (Vfs.resolvedPath <$> resolved)

-- | Fetch source from the debug output using the .source symlink.
-- The .source symlink points to a directory containing the source tree.
-- Uses fuzzy matching to find the requested file.
fetchSourceFromDebugOutput :: DebugInfodEnv -> BuildId -> FilePath -> IO (Maybe FilePath)
fetchSourceFromDebugOutput env buildId requestPath = do
  entryDir <- fetchDebugOutput env buildId
  case entryDir of
    Nothing -> pure Nothing
    Just dir -> do
      -- Resolve the .source symlink (may point to a store path)
      let sourceRp = Vfs.RestrictedPath dir (dir </> buildIdToSourcePath buildId)
      sourceResolved <- Vfs.resolve sourceRp (storePathResolver env)
      case sourceResolved of
        Nothing -> pure Nothing
        Just sdir -> do
          -- Resolve the .sourceoverlay symlink, falling back to source dir
          let overlayRp = Vfs.RestrictedPath dir (dir </> buildIdToSourceOverlayPath buildId)
          overlayResolved <- Vfs.resolve overlayRp (storePathResolver env)
          let overlay = maybe (Vfs.resolvedPath sdir) Vfs.resolvedPath overlayResolved
          SourceSelection.findSourceFile (Vfs.resolvedPath sdir) overlay requestPath

-- | Build a VFS store path resolver from the environment.
storePathResolver :: DebugInfodEnv -> Vfs.StorePathResolver
storePathResolver env sp = do
  mDir <- fetchStorePathOutput env sp
  pure $
    mDir <&> \dir ->
      Vfs.RestrictedPath dir dir

-- | Fetch and cache the debug output NAR for a build ID.
fetchDebugOutput :: DebugInfodEnv -> BuildId -> IO (Maybe FilePath)
fetchDebugOutput env buildId = do
  let bid = buildIdText buildId
      fc = envFetcherCache env
  Cache.fetchOrGet fc bid $ \partialDir -> do
    redirect <- BinaryCache.fetchDebugInfoRedirect env bid
    case redirect of
      Nothing -> pure False
      Just DebugInfoRedirect {archive} -> do
        narData <- BinaryCache.streamNar env archive
        case narData of
          Nothing -> pure False
          Just nar -> do
            Nar.unpackNar archive nar partialDir
            pure True

-- | Fetch and cache a store path via narinfo.
fetchStorePathOutput :: DebugInfodEnv -> StorePath.StorePath -> IO (Maybe FilePath)
fetchStorePathOutput env sp = do
  let spHash = StorePath.storePathHash sp
      sc = envStoreCache env
  Cache.fetchOrGet sc spHash $ \partialDir -> do
    result <- BinaryCache.fetchStorePathNar env spHash
    case result of
      Nothing -> pure False
      Just (narUrl, narData) -> do
        Nar.unpackNar narUrl narData partialDir
        pure True
