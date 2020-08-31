{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.Config
  ( Config (binaryCaches),
    getAuthToken,
    setAuthToken,
    noAuthTokenError,
    BinaryCacheConfig (..),
    readConfig,
    writeConfig,
    getDefaultFilename,
    ConfigPath,
    mkConfig,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Exception (CachixException (..))
import Data.String.Here
import Dhall hiding (Text)
import Dhall.Pretty (prettyExpr)
import Protolude
import Servant.Auth.Client
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
  )
import System.Environment (lookupEnv)
import System.FilePath.Posix (takeDirectory)
import System.Posix.Files
  ( ownerReadMode,
    ownerWriteMode,
    setFileMode,
    unionFileModes,
  )

data BinaryCacheConfig
  = BinaryCacheConfig
      { name :: Text,
        secretKey :: Text
      }
  deriving (Show, Generic, Interpret, Inject)

data Config
  = Config
      { authToken :: Token,
        binaryCaches :: [BinaryCacheConfig]
      }
  deriving (Show, Generic, Interpret, Inject)

mkConfig :: Text -> Config
mkConfig token =
  Config
    { authToken = Token (toS token),
      binaryCaches = []
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

getAuthToken :: Maybe Config -> IO Token
getAuthToken maybeConfig = do
  maybeAuthToken <- lookupEnv "CACHIX_AUTH_TOKEN"
  case (maybeAuthToken, maybeConfig) of
    (Just token, _) -> return $ Token $ toS token
    (Nothing, Just cfg) -> return $ authToken cfg
    (_, _) -> throwIO $ NoConfig $ toS noAuthTokenError

noAuthTokenError :: Text
noAuthTokenError =
  [iTrim|
Start by visiting https://app.cachix.org and either:

a) Via environment variable: 

$ export CACHIX_AUTH_TOKEN=<token...>

b) Running:

$ cachix authtoken <token...>
  |]

setAuthToken :: Config -> Token -> Config
setAuthToken cfg token = cfg {authToken = token}
