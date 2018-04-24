{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Cachix.Client.Config
  ( Config(..)
  , BinaryCacheConfig(..)
  , readConfig
  , writeConfig
  , mkConfig
  ) where

import Dhall     hiding ( Text )
import Dhall.Pretty     ( prettyExpr )
import GHC.Generics     ( Generic )
import System.Directory ( doesFileExist, createDirectoryIfMissing
                        , getXdgDirectory, XdgDirectory(..)
                        )
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

readConfig :: IO (Maybe Config)
readConfig = do
  filename <- getFilename
  doesExist <- doesFileExist filename
  if doesExist
  then Just <$> input auto (toS filename)
  else return Nothing


getFilename :: IO FilePath
getFilename = do
  dir <- getXdgDirectory XdgConfig "cachix"
  createDirectoryIfMissing True dir
  return $ dir <> "/cachix.dhall"

writeConfig :: Config -> IO ()
writeConfig config = do
  filename <- getFilename
  let doc = prettyExpr $ embed inject config
  writeFile filename $ show doc
  putStrLn $ "Written to " <> filename
