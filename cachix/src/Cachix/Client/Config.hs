{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Cachix.Client.Config
  ( Config(..)
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


data Config = Config
 { authToken :: Text
 } deriving (Show, Generic, Interpret, Inject)

mkConfig :: Text -> Config
mkConfig authtoken = Config
  { authToken = authtoken
  }

readConfig :: IO (Maybe Config)
readConfig = do
  filename <- getFilename
  doesExist <- doesFileExist filename
  case doesExist of
    True -> Just <$> input auto (toS filename)
    False -> return Nothing

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
