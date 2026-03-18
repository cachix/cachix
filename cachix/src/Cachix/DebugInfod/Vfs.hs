module Cachix.DebugInfod.Vfs
  ( RestrictedPath (..),
    ResolvedPath,
    StorePathResolver,
    resolve,
    resolveInsideRoot,
    resolvedPath,
    splitComponents,
  )
where

import Cachix.DebugInfod.StorePath qualified as StorePath
import GHC.IO.Exception (IOErrorType (InvalidArgument))
import Protolude hiding (toS)
import Protolude.Conv
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (IOError, ioeGetErrorType, isDoesNotExistError, userError)
import System.Posix.Files (readSymbolicLink)

-- | A path with untrusted symlinks.
-- Non store path symlinks in 'restrictedInner' may not escape 'restrictedRoot'.
data RestrictedPath = RestrictedPath
  { restrictedRoot :: FilePath,
    restrictedInner :: FilePath
  }
  deriving (Show, Eq)

-- | A path where all untrusted symlinks have been resolved.
-- Guaranteed to exist.
newtype ResolvedPath = ResolvedPath FilePath
  deriving (Show, Eq)

-- | Callback to fetch a store path output. Returns a RestrictedPath rooted
-- at the fetched output directory. Returns Nothing if unavailable.
type StorePathResolver = StorePath.StorePath -> IO (Maybe RestrictedPath)

maxSymlinkDepth :: Int
maxSymlinkDepth = 20

-- | Extract the underlying file path.
resolvedPath :: ResolvedPath -> FilePath
resolvedPath (ResolvedPath p) = p

-- | Resolve all symlinks in a RestrictedPath.
-- Symlinks must either stay within the root or point to /nix/store/ paths
-- (which are resolved via the callback).
resolve :: RestrictedPath -> StorePathResolver -> IO (Maybe ResolvedPath)
resolve rp resolver = resolveLoop (restrictedRoot rp) (restrictedInner rp) 0
  where
    resolveLoop :: FilePath -> FilePath -> Int -> IO (Maybe ResolvedPath)
    resolveLoop root toBeResolved depth = do
      when (depth > maxSymlinkDepth) $
        throwIO $
          userError $
            "more than " <> show maxSymlinkDepth <> " symlinks resolving " <> restrictedInner rp
      case stripPathPrefix root toBeResolved of
        Nothing ->
          throwIO $ userError $ toBeResolved <> " escaped out of " <> root
        Just relParts ->
          walkComponents root root relParts depth

    walkComponents :: FilePath -> FilePath -> [FilePath] -> Int -> IO (Maybe ResolvedPath)
    walkComponents _root resolved [] _depth =
      pure (Just (ResolvedPath resolved))
    walkComponents root resolved (comp : rest) depth
      | comp == "." =
          walkComponents root resolved rest depth
      | comp == ".." = do
          isDir <- doesDirectoryExist resolved
          unless isDir $
            throwIO $
              userError $
                resolved <> " is not a directory"
          let parent = takeDirectory resolved
          unless (isPathPrefix root parent) $
            throwIO $
              userError $
                restrictedInner rp <> " escaped out of " <> root
          walkComponents root parent rest depth
      | otherwise = do
          let next = resolved </> comp
          readLinkSafe next >>= \case
            Left NotFound -> pure Nothing
            Left NotASymlink -> walkComponents root next rest depth
            Left (OtherError e) -> throwIO e
            Right target -> do
              let base = takeDirectory next
                  joined = foldl' (</>) (base </> target) rest
              if "/nix/store/" `isPrefixOf` joined
                then case StorePath.parseStorePath (toS joined) of
                  Left err ->
                    throwIO $
                      userError $
                        restrictedInner rp <> " resolves to malformed store path: " <> toS err
                  Right sp -> do
                    result <- resolver sp
                    case result of
                      Nothing -> pure Nothing
                      Just newRp ->
                        let rel = StorePath.storePathRelative sp
                            newInner
                              | null rel = restrictedRoot newRp
                              | otherwise = restrictedRoot newRp </> rel
                         in resolveLoop (restrictedRoot newRp) newInner (depth + 1)
                else resolveLoop root joined (depth + 1)

-- | Like 'resolve' but errors if any symlink points to a store path.
resolveInsideRoot :: RestrictedPath -> IO (Maybe ResolvedPath)
resolveInsideRoot rp =
  resolve rp $ \sp ->
    throwIO $
      userError $
        "not allowed to point to store path " <> show sp

-- Internal

data ReadLinkResult
  = NotFound
  | NotASymlink
  | OtherError IOError

readLinkSafe :: FilePath -> IO (Either ReadLinkResult FilePath)
readLinkSafe path = do
  result <- try @IOError $ readSymbolicLink path
  pure $ case result of
    Right target -> Right target
    Left e
      | isDoesNotExistError e -> Left NotFound
      | ioeGetErrorType e == InvalidArgument -> Left NotASymlink
      | otherwise -> Left (OtherError e)

splitComponents :: FilePath -> [FilePath]
splitComponents = filter (not . null) . go
  where
    go :: FilePath -> [FilePath]
    go [] = []
    go s =
      let (comp, rest') = break (== '/') s
          rest'' = dropWhile (== '/') rest'
       in if null comp then go rest'' else comp : go rest''

isPathPrefix :: FilePath -> FilePath -> Bool
isPathPrefix root path =
  let rootParts = splitComponents root
      pathParts = splitComponents path
   in rootParts `isPrefixOf` pathParts || null rootParts

stripPathPrefix :: FilePath -> FilePath -> Maybe [FilePath]
stripPathPrefix root path =
  let rootParts = splitComponents root
      pathParts = splitComponents path
   in if rootParts `isPrefixOf` pathParts
        then Just (drop (length rootParts) pathParts)
        else Nothing
