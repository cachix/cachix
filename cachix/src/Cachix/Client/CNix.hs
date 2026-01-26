module Cachix.Client.CNix
  ( -- * Store path validation
    StorePathError (..),
    resolveStorePath,
    resolveStorePaths,
    validateStorePath,
    formatStorePathWarning,
    logStorePathWarning,
    logStorePathWarning',

    -- * Legacy API (deprecated)
    filterInvalidStorePath,
    filterInvalidStorePaths,
    followLinksToStorePath,

    -- * Error handling
    NixError (..),
    catchNixError,
    handleCppExceptions,
  )
where

import Hercules.CNix.Store (Store, StorePath)
import Hercules.CNix.Store qualified as Store
import Language.C.Inline.Cpp.Exception
import Protolude
import System.Console.Pretty (Color (..), Style (..), color, style)

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
  let pathBytes = encodeUtf8 (toS fp :: Text)
  resolveResult <- tryResolve pathBytes
  case resolveResult of
    Left err -> pure $ Left (StorePathNotFound (msg err))
    Right storePath -> validateStorePath store storePath
  where
    tryResolve pathBytes =
      (Right <$> Store.followLinksToStorePath store pathBytes)
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
  pathBytes <- Store.storePathToPath store storePath
  let path = toS (decodeUtf8With lenientDecode pathBytes :: Text)
  logStorePathWarning path err

-- | Validate that an existing store path is still valid in the Nix store.
--
-- Use this to re-check a previously resolved path, e.g., before pushing
-- a path that may have been garbage collected since it was queued.
validateStorePath :: Store -> StorePath -> IO (Either StorePathError StorePath)
validateStorePath store storePath = do
  result <- tryValidate
  case result of
    Left err -> pure $ Left (StorePathError (msg err))
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
followLinksToStorePath store storePath =
  (Just <$> Store.followLinksToStorePath store storePath)
    `catchNixError` \e -> do
      logNixError storePath e
      return Nothing

data NixError = NixError
  { typ :: Maybe Text,
    msg :: Text
  }
  deriving (Show)

-- | Capture and handle Nix errors
catchNixError :: IO a -> (NixError -> IO a) -> IO a
catchNixError f onError =
  handleJust selectNixError onError f
  where
    selectNixError (CppStdException _eptr msg typ) =
      Just $
        NixError
          { typ = decodeUtf8With lenientDecode <$> typ,
            msg = decodeUtf8With lenientDecode msg
          }
    selectNixError _ = Nothing

-- | Pretty-print a Nix error.
--
-- There might not be a valid store path at this point.
logNixError :: ByteString -> NixError -> IO ()
logNixError storePath (NixError {..}) =
  case typ of
    Just "nix::BadStorePath" -> logBadPath storePath
    _ -> putErrText $ color Red $ style Bold $ "Nix " <> msg

-- | Print a warning when the path is invalid.
--
-- There are two use-cases when this should be used:
-- 1. Converting a file path to a store path.
-- 2. Filtering out a store path that is not valid.
logBadPath :: ByteString -> IO ()
logBadPath path =
  putErrText $ color Yellow $ "Warning: " <> decodeUtf8With lenientDecode path <> " is not valid, skipping"

-- | Pretty-print unhandled C++ exceptions from Nix.
handleCppExceptions :: CppException -> IO ()
handleCppExceptions e = do
  putErrText ""

  case e of
    CppStdException _eptr msg _t ->
      putErrText $ color Red $ style Bold $ "Nix " <> decodeUtf8With lenientDecode msg
    CppNonStdException _eptr mmsg -> do
      let msg = fromMaybe "unknown exception" mmsg
      putErrText $ color Red $ style Bold $ "C++ exception: " <> decodeUtf8With lenientDecode msg
    CppHaskellException he ->
      putErrText $ color Red $ style Bold $ "Haskell exception: " <> toS (displayException he)

  exitFailure
