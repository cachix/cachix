{-# LANGUAGE OverloadedStrings #-}

module Cachix.Client.Store
  ( -- * Opening a store
    LocalStoreOptions (..),
    withLocalStore,
    withStore,
    Store (..),

    -- * Working store contents
    PathInfo (..),
    StorePath (..),
    getUseSqliteWAL,
    base16to32,
    computeClosure,
    queryPathInfo,
    followLinksToStorePath,
    getStorePathHash,
    getStorePathBaseName,
    getPath,
  )
where

import Cachix.Client.ProcessGraph (processGraph)
import Data.ByteArray.Encoding (Base (..), convertFromBase)
import qualified Data.Set as Set
import qualified Data.Text as T
import Database.SQLite3 (SQLData)
import qualified Database.SQLite3 as SQLite
import Protolude hiding (toS)
import Protolude.Conv
import System.Console.Pretty (Color (..), color)
import System.Directory (canonicalizePath)
import qualified System.Nix.Base32
import System.Process (readProcessWithExitCode)

type StorePrefix = Text

data Store = Store StorePrefix SQLite.Database

data StorePath = StorePath Text
  deriving (Eq, Ord)

data PathInfo = PathInfo
  { deriver :: Maybe Text,
    narSize :: Int64,
    narHash :: Text,
    references :: [Text]
  }

followLinksToStorePath :: Text -> FilePath -> IO FilePath
followLinksToStorePath prefix path = do
  storePath <- canonicalizePath path
  let storePath' = T.drop (T.length prefix) (toS storePath)
  return $ toS $ prefix <> T.intercalate "/" (take 3 $ T.splitOn "/" storePath')

-- | Options for `withLocalStore`.
data LocalStoreOptions = LocalStoreOptions
  { -- | The path to the Nix store directory. Typically: @"/nix"@
    storePrefix :: !Text,
    -- | Whether to use SQLite Write-Ahead Logging (WAL) mode.
    --
    -- This needs to match the ambient configuration, because otherwise, the db
    -- may be corrupted: https://github.com/cachix/cachix/issues/475
    useSqliteWAL :: !Bool
  }

-- | Run an 'IO' action while retaining a 'Store' resource for the duration of the action.
--
-- This does not rely on the @nix@ command being available.
withLocalStore :: LocalStoreOptions -> (Store -> IO a) -> IO a
withLocalStore opts =
  bracket open close
  where
    uri = "file:" <> toS (storePrefix opts) <> "/var/nix/db/db.sqlite?immutable=1"
    flags = [SQLite.SQLOpenReadOnly, SQLite.SQLOpenURI]
    close (Store _ db) = SQLite.close db
    vfs =
      if useSqliteWAL opts
        then SQLite.SQLVFSDefault
        else SQLite.SQLVFSUnixDotFile
    open = do
      conn <- SQLite.open2 uri flags vfs
      return $ Store (storePrefix opts) conn

-- | 'withLocalStore' but infers 'useSqliteWAL' from the @nix show-config@ command.
withStore :: Text -> (Store -> IO a) -> IO a
withStore storePrefix_ f = do
  wal <- getUseSqliteWAL
  withLocalStore
    LocalStoreOptions
      { storePrefix = storePrefix_,
        useSqliteWAL = wal
      }
    f

getUseSqliteWAL :: IO Bool
getUseSqliteWAL = do
  (_, out, _) <- readProcessWithExitCode "nix" ["show-config", "--extra-experimental-features", "nix-command"] mempty
  pure (not ("use-sqlite-wal = false" `T.isInfixOf` toS out))

queryNarinfo :: Text
queryNarinfo = "select id, hash, deriver, narSize from ValidPaths where path = :path"

queryReferences :: Text
queryReferences = "select path from Refs join ValidPaths on reference = id where referrer = :id"

query :: Store -> Text -> [(Text, SQLData)] -> IO [[SQLite.SQLData]]
query (Store _ conn) txt bindings =
  bracket (SQLite.prepare conn txt) SQLite.finalize $ \stmt -> do
    SQLite.bindNamed stmt bindings
    getRows stmt

getRows :: SQLite.Statement -> IO [[SQLite.SQLData]]
getRows stmt = do
  SQLite.step stmt >>= \case
    SQLite.Row -> do
      row <- SQLite.columns stmt
      rows <- getRows stmt
      return $ row : rows
    SQLite.Done -> do
      return []

queryPathInfo :: Store -> Text -> IO (Either Text PathInfo)
queryPathInfo store path = do
  rows <- query store queryNarinfo [(":path", SQLite.SQLText path)]
  case rows of
    [] -> return $ Left $ "no such path " <> path
    [[id_, SQLite.SQLText hash_, deriver, SQLite.SQLInteger narSize]] -> do
      references <- query store queryReferences [(":id", id_)]
      refs <- traverse go references
      return $
        Right $
          PathInfo
            { deriver = getDeriver deriver,
              narSize = narSize,
              narHash = hash_,
              references = refs
            }
    _ -> return $ Left $ "got invalid narinfo from nix " <> show rows
  where
    go [SQLite.SQLText path_] = return path_
    go a = throwIO $ FatalError $ "invalid reference type " <> show a

    getDeriver :: SQLite.SQLData -> Maybe Text
    getDeriver (SQLite.SQLText deriver) = Just deriver
    getDeriver _ = Nothing

computeClosure :: Store -> [StorePath] -> IO [StorePath]
computeClosure store initialPaths = do
  allPaths <-
    processGraph (getPath <$> initialPaths) $ \path -> do
      queryPathInfo store path >>= \case
        Left _ -> do
          hPutStrLn stderr $ color Yellow $ "Warning: " <> path <> " is not valid, skipping"
          return []
        Right pathInfo -> pure $ references pathInfo
  return $ StorePath <$> Set.toList allPaths

getStorePathHash :: Store -> StorePath -> Text
getStorePathHash store storePath =
  T.take 32 $ getStorePathBaseName store storePath

getPath :: StorePath -> Text
getPath (StorePath storePath) = storePath

getStorePathBaseName :: Store -> StorePath -> Text
getStorePathBaseName (Store storePrefix _) (StorePath storePath) =
  dropPrefix (dropSuffix "/" storePrefix <> "/store/") storePath
  where
    dropPrefix :: Text -> Text -> Text
    dropPrefix prefix str =
      fromMaybe str (T.stripPrefix prefix str)

    dropSuffix :: Text -> Text -> Text
    dropSuffix suffix str =
      fromMaybe str (T.stripSuffix suffix str)

base16to32 :: Text -> Either Text Text
base16to32 path =
  case T.splitOn ":" path of
    [_, path_] -> convert path_
    [] -> convert path
    _ -> Left $ "can't split : for " <> path
  where
    convert :: Text -> Either Text Text
    convert stripped =
      case convertFromBase Base16 (toS stripped :: ByteString) of
        Left err -> Left $ toS err
        Right decoded -> Right $ ("sha256:" <>) $ System.Nix.Base32.encode decoded
