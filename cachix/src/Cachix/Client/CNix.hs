module Cachix.Client.CNix
  ( -- * Store path validation
    StorePathError (..),
    resolveStorePath,
    resolveStorePaths,
    validateStorePath,
    formatStorePathError,
    formatStorePathWarning,
    logStorePathWarning,
    logStorePathWarning',

    -- * Legacy API (deprecated)
    filterInvalidStorePath,
    filterInvalidStorePaths,
    followLinksToStorePath,

    -- * Error handling
    catchNixError,
    handleNixExceptions,
  )
where

import Nix.C.Context (NixError (..))
import Nix.C.Unsafe.Store (Store, StorePath)
import Nix.C.Unsafe.Store qualified as Store
import Protolude
import System.Console.Pretty (Color (..), Style (..), color, style)
import System.Directory (canonicalizePath)
import System.OsPath qualified as OsPath

-- | Error when resolving a store path
data StorePathError
  = -- | Path could not be resolved (doesn't exist, bad symlink, etc.)
    StorePathNotFound Text
  | -- | Path exists but is not valid in the Nix store
    StorePathNotValid
  | -- | Error occurred during validation (e.g., permission denied)
    StorePathError Text
  deriving (Show, Eq)

-- | Resolve a file path to a validated store path.
--
-- Follows symlinks and validates the resulting store path.
resolveStorePath :: Store -> FilePath -> IO (Either StorePathError StorePath)
resolveStorePath store fp = do
  resolveResult <- tryResolve
  case resolveResult of
    Left err -> pure $ Left (StorePathNotFound (nixErrorMsg err))
    Right storePath -> validateStorePath store storePath
  where
    tryResolve = do
      resolved <- canonicalizePath fp
      osPath <- OsPath.encodeFS resolved
      (Right <$> Store.parseStorePath' store osPath)
        `catchNixError` (pure . Left)

-- | Resolve multiple file paths to validated store paths.
--
-- Returns a pair of (errors, valid paths). Errors are paired with their
-- original file path for error reporting.
resolveStorePaths :: Store -> [FilePath] -> IO ([(FilePath, StorePathError)], [StorePath])
resolveStorePaths store paths = do
  results <- mapM (resolveStorePath store) paths
  pure $ foldr go ([], []) (zip paths results)
  where
    go (path, Left err) (errs, oks) = ((path, err) : errs, oks)
    go (_, Right sp) (errs, oks) = (errs, sp : oks)

-- | Format a store path error as a reason string (without the path).
formatStorePathError :: StorePathError -> Text
formatStorePathError = \case
  StorePathNotFound reason -> reason
  StorePathNotValid -> "not valid"
  StorePathError reason -> reason

-- | Format a store path error as a user-friendly warning message.
formatStorePathWarning :: FilePath -> StorePathError -> Text
formatStorePathWarning path = \case
  StorePathNotFound reason -> toS path <> ": " <> reason
  StorePathNotValid -> toS path <> " is not valid"
  StorePathError reason -> toS path <> ": " <> reason

-- | Log a store path error as a warning (yellow, to stderr).
logStorePathWarning :: FilePath -> StorePathError -> IO ()
logStorePathWarning path err =
  putErrText $ color Yellow $ "Warning: " <> formatStorePathWarning path err <> ", skipping"

-- | Log a store path validation error as a warning.
--
-- Like 'logStorePathWarning' but for when you have a 'StorePath' instead of a 'FilePath'.
logStorePathWarning' :: Store -> StorePath -> StorePathError -> IO ()
logStorePathWarning' store storePath err = do
  osPath <- Store.storeRealPath store storePath
  path <- OsPath.decodeFS osPath
  logStorePathWarning path err

-- | Validate that an existing store path is still valid in the Nix store.
--
-- Use this to re-check a previously resolved path, e.g., before pushing
-- a path that may have been garbage collected since it was queued.
validateStorePath :: Store -> StorePath -> IO (Either StorePathError StorePath)
validateStorePath store storePath = do
  result <- tryValidate
  case result of
    Left err -> pure $ Left (StorePathError (nixErrorMsg err))
    Right True -> pure $ Right storePath
    Right False -> pure $ Left StorePathNotValid
  where
    tryValidate =
      (Right <$> Store.isValidPath store storePath)
        `catchNixError` (pure . Left)

{-# DEPRECATED filterInvalidStorePath "Use validateStorePath + logStorePathWarning' instead" #-}

-- | Like 'validateStorePath', but logs a warning when the path is invalid.
filterInvalidStorePath :: Store -> StorePath -> IO (Maybe StorePath)
filterInvalidStorePath store storePath =
  validateStorePath store storePath >>= \case
    Right sp -> return (Just sp)
    Left err -> do
      logStorePathWarning' store storePath err
      return Nothing

{-# DEPRECATED filterInvalidStorePaths "Use resolveStorePath + logStorePathWarning instead" #-}
filterInvalidStorePaths :: Store -> [StorePath] -> IO [Maybe StorePath]
filterInvalidStorePaths store =
  traverse (filterInvalidStorePath store)

{-# DEPRECATED followLinksToStorePath "Use resolveStorePath instead" #-}

-- | Follows all symlinks to a store path, returning the final store path.
--
-- Returns Nothing if the path is invalid.
followLinksToStorePath :: Store -> ByteString -> IO (Maybe StorePath)
followLinksToStorePath store storePathBS = do
  let filePath = toS (decodeUtf8With lenientDecode storePathBS :: Text)
  resolveStorePath store filePath >>= \case
    Right sp -> return (Just sp)
    Left err -> do
      logStorePathWarning filePath err
      return Nothing

-- | Extract a human-readable message from a NixError.
nixErrorMsg :: NixError -> Text
nixErrorMsg (NixCError _ msg _info) = decodeUtf8With lenientDecode msg
nixErrorMsg (NixTypeMismatch expected actual) = "type mismatch: expected " <> show expected <> ", got " <> show actual
nixErrorMsg (NixMissingAttr name) = "missing attribute: " <> decodeUtf8With lenientDecode name
nixErrorMsg (NixIndexOutOfBounds idx) = "index out of bounds: " <> show idx

-- | Capture and handle Nix errors
catchNixError :: IO a -> (NixError -> IO a) -> IO a
catchNixError f onError =
  f `catch` onError

-- | Pretty-print unhandled Nix exceptions.
handleNixExceptions :: NixError -> IO ()
handleNixExceptions e = do
  putErrText ""
  putErrText $ color Red $ style Bold $ "Nix error: " <> nixErrorMsg e
  exitFailure
