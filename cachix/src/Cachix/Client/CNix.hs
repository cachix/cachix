module Cachix.Client.CNix where

import Hercules.CNix.Store (Store, StorePath)
import qualified Hercules.CNix.Store as Store
import Language.C.Inline.Cpp.Exception
import Protolude
import System.Console.Pretty (Color (..), Style (..), color, style)

-- | Checks whether a store path is valid.
validateStorePath :: Store -> StorePath -> IO (Maybe StorePath)
validateStorePath store storePath = do
  isValid <- Store.isValidPath store storePath `catchNixError` const (return False)
  if isValid
    then return (Just storePath)
    else return Nothing

-- | Like 'validateStorePath', but logs a warning when the path is invalid.
filterInvalidStorePath :: Store -> StorePath -> IO (Maybe StorePath)
filterInvalidStorePath store storePath = do
  mstorePath <- validateStorePath store storePath

  when (isNothing mstorePath) (logBadStorePath store storePath)

  return mstorePath

filterInvalidStorePaths :: Store -> [StorePath] -> IO [Maybe StorePath]
filterInvalidStorePaths store =
  traverse (filterInvalidStorePath store)

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

-- | Print a warning when a store path is invalid.
logBadStorePath :: Store -> StorePath -> IO ()
logBadStorePath store storePath = do
  path <- Store.storePathToPath store storePath
  logBadPath path

-- | Print a warning when the path is invalid.
--
-- There are two use-cases when this should be used:
-- 1. Converting a file path to a store path.
-- 2. Filtering out a store path that is not valid.
logBadPath :: ByteString -> IO ()
logBadPath path =
  putErrText $ color Yellow $ "Warning: " <> decodeUtf8With lenientDecode path <> " is not valid, skipping"
