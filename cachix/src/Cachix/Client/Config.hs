{-# LANGUAGE DeriveAnyClass #-}
module Cachix.Client.Config
  ( Config(..)
  , BinaryCacheConfig(..)
  , readConfig
  , writeConfig
  , getDefaultFilename
  , ConfigPath
  , mkConfig
  ) where

import Dhall     hiding ( Text )
import Dhall.Pretty     ( prettyExpr )
import GHC.Generics     ( Generic )
import System.Directory ( doesFileExist, createDirectoryIfMissing
                        , getXdgDirectory, XdgDirectory(..)
                        )
import System.Posix.Files ( setFileMode, unionFileModes, ownerReadMode
                          , ownerWriteMode )
import Protolude


data BinaryCacheConfig = BinaryCacheConfig
 { name :: Text
 , secretKey :: Text
 } deriving (Show, Generic, Interpret, Inject)

data Config = Config
 { authToken :: Text
 , binaryCaches :: [BinaryCacheConfig]
 } deriving (Show, Generic, Interpret, Inject)

mkConfig :: Text -> Config
mkConfig authtoken = Config
  { authToken = authtoken
  , binaryCaches = []
  }

type ConfigPath = FilePath

readConfig :: ConfigPath -> IO (Maybe Config)
readConfig filename = do
  doesExist <- doesFileExist filename
  if doesExist
  then Just <$> input auto (toS filename)
  else return Nothing

getDefaultFilename :: IO FilePath
getDefaultFilename = do
  dir <- getXdgDirectory XdgConfig "cachix"
  createDirectoryIfMissing True dir
  return $ dir <> "/cachix.dhall"

writeConfig :: ConfigPath -> Config -> IO ()
writeConfig filename config = do
  let doc = prettyExpr $ embed inject config
  writeFile filename $ show doc
  assureFilePermissions filename
  putStrLn $ "Written to " <> filename

-- Does: chmod rw file
assureFilePermissions :: FilePath -> IO ()
assureFilePermissions fp =
  setFileMode fp $ unionFileModes ownerReadMode ownerWriteMode
