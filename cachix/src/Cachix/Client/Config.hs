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
import Dhall.Core (Expr(..), Chunks(..))
import GHC.Generics     ( Generic )
import Servant.Auth.Client
import System.Directory ( doesFileExist, createDirectoryIfMissing
                        , getXdgDirectory, XdgDirectory(..)
                        )
import System.Posix.Files ( setFileMode, unionFileModes, ownerReadMode
                          , ownerWriteMode )
import System.FilePath.Posix ( takeDirectory )
import Protolude


data BinaryCacheConfig = BinaryCacheConfig
 { name :: Text
 , secretKey :: Text
 } deriving (Show, Generic, Interpret, Inject)

data Config = Config
 { authToken :: Token
 , binaryCaches :: [BinaryCacheConfig]
 } deriving (Show, Generic, Interpret, Inject)

instance Interpret Token where
  autoWith _ = Type
    { extract = \(TextLit (Chunks [] t)) -> pure (Token (toS t))
    , expected = Text
    }

instance Inject Token where
  injectWith _ = InputType
    { embed = TextLit . Chunks [] . show
    , declared = Text
    }

mkConfig :: Text -> Config
mkConfig token = Config
  { authToken = Token (toS token)
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
  return $ dir <> "/cachix.dhall"

writeConfig :: ConfigPath -> Config -> IO ()
writeConfig filename config = do
  let doc = prettyExpr $ embed inject config
  createDirectoryIfMissing True (takeDirectory filename)
  writeFile filename $ show doc
  assureFilePermissions filename
  putStrLn $ "Written to " <> filename

-- chmod rw filepath
assureFilePermissions :: FilePath -> IO ()
assureFilePermissions fp =
  setFileMode fp $ unionFileModes ownerReadMode ownerWriteMode
