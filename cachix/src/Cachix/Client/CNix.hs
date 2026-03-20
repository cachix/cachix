module Cachix.Client.CNix
  ( -- * Store path parsing
    splitStorePath,
    extractStoreHash,

    -- * Store path validation
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

    -- * Store path filesystem mapping
    storePathToRealPath,

    -- * Store connection
    withStoreFromMaybeURI,

    -- * Error handling
    NixError (..),
    catchNixError,
    handleCppExceptions,
  )
where

import Data.Text qualified as T
import Hercules.CNix.Store (Store, StorePath)
import Hercules.CNix.Store qualified as Store
import Language.C.Inline.Cpp.Exception
import Data.List (lookup)
import Protolude hiding (toS)
import Protolude.Conv
import System.Console.Pretty (Color (..), Style (..), color, style)
import System.FilePath ((</>), makeRelative)
import URI.ByteString qualified as URI

-- | Split a store path into its hash and suffix components.
--
-- >>> splitStorePath "/nix/store" "/nix/store/abc...-foo"
-- ("abc...", "foo")
splitStorePath :: FilePath -> Text -> (Text, Text)
splitStorePath storeDirectory storePathText =
  let prefixLen = length storeDirectory + 1
      rest = T.drop prefixLen storePathText
      storeHash = T.take 32 rest
      storeSuffix = T.drop 33 rest
   in (storeHash, storeSuffix)

-- | Extract the hash from a store path or bare hash.
--
-- Accepts full store paths (e.g. @\/nix\/store\/abc...-name@) or bare
-- 32-character hashes.
extractStoreHash :: FilePath -> Text -> Maybe Text
extractStoreHash storeDirectory input =
  let path = T.strip input
      prefix = toS storeDirectory <> "/"
   in if prefix `T.isPrefixOf` path
        then
          let (storeHash, _) = splitStorePath storeDirectory path
           in guard (T.length storeHash == 32) $> storeHash
        else guard (T.length path >= 32) $> T.take 32 path

-- | Get the real filesystem path for a store path.
--
-- For the default store, this is the same as @storePathToPath@.
-- For local stores with a custom root (e.g. @\/tmp\/hello@ or
-- @local?root=\/tmp\/hello@), this returns the physical path
-- (e.g. @\/tmp\/hello\/nix\/store\/hash-name@).
--
-- Only local stores are supported. Non-local store URIs are not expected
-- to have a meaningful filesystem path.
storePathToRealPath :: Maybe Text -> Store -> StorePath -> IO FilePath
storePathToRealPath storeURI store storePath = do
  logicalPath <- toS <$> Store.storePathToPath store storePath
  pure $ case parseStoreRoot storeURI of
    Nothing -> logicalPath
    Just root -> root </> makeRelative "/" logicalPath

-- | Parse the store root from a Nix store URI as provided by the user.
--
-- Accepts:
--
--   * Bare absolute paths: @\/tmp\/hello@
--   * Local store URIs with a root parameter: @local?root=\/tmp\/hello@
--
-- Returns @Nothing@ for the default store or non-local store URIs.
--
-- >>> parseStoreRoot Nothing
-- Nothing
-- >>> parseStoreRoot (Just "/tmp/hello")
-- Just "/tmp/hello"
-- >>> parseStoreRoot (Just "local?root=/tmp/hello")
-- Just "/tmp/hello"
-- >>> parseStoreRoot (Just "local?root=/tmp/hello&real=/somewhere")
-- Just "/tmp/hello"
-- >>> parseStoreRoot (Just "local")
-- Nothing
-- >>> parseStoreRoot (Just "daemon")
-- Nothing
parseStoreRoot :: Maybe Text -> Maybe FilePath
parseStoreRoot Nothing = Nothing
parseStoreRoot (Just uri)
  -- Bare absolute path, e.g. /tmp/hello
  | "/" `T.isPrefixOf` uri = Just (toS uri)
  -- URI with query params, e.g. local?root=/tmp/hello
  | otherwise = do
      ref <- either (const Nothing) Just $ URI.parseRelativeRef URI.laxURIParserOptions (toS uri)
      guard (URI.rrPath ref == "local")
      let params = URI.queryPairs (URI.rrQuery ref)
      root <- lookup "root" params
      pure (toS root)

-- | Open a Nix store, using the given URI if provided, or the default store.
withStoreFromMaybeURI :: Maybe Text -> (Store -> IO a) -> IO a
withStoreFromMaybeURI Nothing f = Store.withStore f
withStoreFromMaybeURI (Just uri) f = Store.withStoreFromURI (toS uri) f

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
