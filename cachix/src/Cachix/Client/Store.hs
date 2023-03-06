{-# LANGUAGE OverloadedStrings #-}

module Cachix.Client.Store (withStore, Store, PathInfo (..), StorePath (..), base16to32, computeClosure, queryPathInfo, followLinksToStorePath, getStorePathHash, getStorePathBaseName, getPath) where

import Cachix.Client.ProcessGraph (processGraph)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import Database.SQLite3 (SQLData)
import qualified Database.SQLite3 as SQLite
import Protolude hiding (toS)
import Protolude.Conv
import System.Directory (canonicalizePath)
import qualified System.Nix.Base32

data Store = Store Text SQLite.Database

data StorePath = StorePath Text
  deriving (Eq, Ord)

data PathInfo = PathInfo
  { deriver :: Text,
    narSize :: Int64,
    narHash :: Text,
    references :: [Text]
  }

followLinksToStorePath :: Store -> FilePath -> IO FilePath
followLinksToStorePath (Store prefix _) path = do
  storePath <- canonicalizePath path
  let storePath' = T.drop (T.length prefix) (toS storePath)
  return $ toS $ prefix <> T.intercalate "/" (take 3 $ T.splitOn "/" storePath')

withStore :: Text -> (Store -> IO ()) -> IO ()
withStore storePrefix =
  bracket open close
  where
    uri = "file:" <> toS storePrefix <> "/var/nix/db/db.sqlite?immutable=1"
    flags = [SQLite.SQLOpenReadOnly, SQLite.SQLOpenURI]
    close (Store _ db) = SQLite.close db
    open = do
      conn <- SQLite.open2 uri flags SQLite.SQLVFSDefault
      return $ Store storePrefix conn

queryNarinfo :: Text
queryNarinfo = "select id, hash, deriver, narSize from ValidPaths where path = :path"

queryReferences :: Text
queryReferences = "select path from Refs join ValidPaths on reference = id where referrer = :id"

query :: Store -> Text -> [(Text, SQLData)] -> IO [[SQLite.SQLData]]
query (Store _ conn) txt bindings = do
  stmt <- SQLite.prepare conn txt
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
      SQLite.finalize stmt
      return []

queryPathInfo :: Store -> Text -> IO (Either Text PathInfo)
queryPathInfo store path = do
  rows <- query store queryNarinfo [(":path", SQLite.SQLText path)]
  case rows of
    [] -> return $ Left $ "no such path " <> path
    [[id_, SQLite.SQLText hash_, SQLite.SQLText deriver, SQLite.SQLInteger narSize]] -> do
      references <- query store queryReferences [(":id", id_)]
      refs <- traverse go references
      return $
        Right $
          PathInfo
            { deriver = deriver,
              narSize = narSize,
              narHash = hash_,
              references = refs
            }
    _ -> return $ Left $ "got invalid narinfo from nix " <> show rows
  where
    go [SQLite.SQLText path_] = return path_
    go a = throwIO $ FatalError $ "invalid reference type " <> show a

computeClosure :: Store -> [StorePath] -> IO [StorePath]
computeClosure store initialPaths = do
  allPaths <- processGraph (getPath <$> initialPaths) (fmap (concatMap references . rights . List.singleton) . queryPathInfo store)
  return $ StorePath <$> Set.toList allPaths

getStorePathHash :: StorePath -> Text
getStorePathHash storePath =
  T.take 32 $ getStorePathBaseName storePath

getPath :: StorePath -> Text
getPath (StorePath storePath) = storePath

getStorePathBaseName :: StorePath -> Text
getStorePathBaseName (StorePath storePath) =
  T.drop 11 storePath

base16to32 :: Text -> Either Text Text
base16to32 path =
  case T.splitOn ":" path of
    [_, path_] -> convert path_
    [] -> convert path
    _ -> Left $ "can't split : for " <> path
  where
    convert :: Text -> Either Text Text
    convert stripped =
      case Base16.decode (toS stripped) of
        Left err -> Left $ toS err
        Right decoded -> Right $ ("sha256:" <>) $ System.Nix.Base32.encode decoded
