{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.Config
  ( Config (binaryCaches),
    getAuthTokenRequired,
    getAuthTokenOptional,
    getAuthTokenMaybe,
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

getAuthTokenRequired :: Maybe Config -> IO Token
getAuthTokenRequired maybeConfig = do
  authTokenMaybe <- getAuthTokenMaybe maybeConfig
  case authTokenMaybe of
    Just authtoken -> return authtoken
    Nothing -> throwIO $ NoConfig $ toS noAuthTokenError

-- TODO: https://github.com/haskell-servant/servant-auth/issues/173
getAuthTokenOptional :: Maybe Config -> IO Token
getAuthTokenOptional maybeConfig = do
  authTokenMaybe <- getAuthTokenMaybe maybeConfig
  return $ Protolude.maybe (Token "") identity authTokenMaybe

-- get auth token from env variable or fallback to config
getAuthTokenMaybe :: Maybe Config -> IO (Maybe Token)
getAuthTokenMaybe maybeConfig = do
  maybeAuthToken <- lookupEnv "CACHIX_AUTH_TOKEN"
  case (maybeAuthToken, maybeConfig) of
    (Just token, _) -> return $ Just $ Token $ toS token
    (Nothing, Just cfg) -> return $ Just $ authToken cfg
    (_, _) -> return Nothing

noAuthTokenError :: Text
noAuthTokenError =
  [iTrim|
Start by visiting https://app.cachix.org and create a personal/cache auth token.

To configure the token:

a) Via environment variable: 

$ export CACHIX_AUTH_TOKEN=<token...>

b) Via configuration file:

$ cachix authtoken <token...>
  |]

setAuthToken :: Config -> Token -> Config
setAuthToken cfg token = cfg {authToken = token}
