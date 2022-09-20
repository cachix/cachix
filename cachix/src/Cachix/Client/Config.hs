{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.Config
  ( Command (..),
    ConfigKey (..),
    Config (binaryCaches),
    parser,
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
import qualified Dhall
import qualified Dhall.Pretty
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as Opt.Help
import Protolude hiding (toS)
import Protolude.Conv
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

data Command
  = Set ConfigKey
  deriving (Show)

data ConfigKey
  = HostName Text
  deriving (Show)

parser :: Opt.ParserInfo Command
parser =
  Opt.info (Opt.helper <*> commandParser) $
    mconcat
      [ Opt.fullDesc,
        Opt.progDesc "Manage the configuration for cachix",
        Opt.footer (toS $ Opt.Help.renderHelp 80 (Opt.Help.bodyHelp printAllConfigurationKeys))
      ]
  where
    printAllConfigurationKeys =
      Opt.Help.vcatChunks $
        map snd $
          Opt.Help.cmdDesc Opt.defaultPrefs $
            Opt.subparser (mconcat supportedConfigKeys)

commandParser :: Opt.Parser Command
commandParser =
  Opt.subparser $
    mconcat
      [ Opt.command "set" $
          Opt.info (Opt.helper <*> configKeyParser Set) $
            Opt.progDesc "Set a configuration option"
      ]

configKeyParser :: (ConfigKey -> Command) -> Opt.Parser Command
configKeyParser cmd = do
  cmd
    <$> Opt.subparser (Opt.metavar "KEY VALUE" <> mconcat supportedConfigKeys)

supportedConfigKeys :: [Opt.Mod Opt.CommandFields ConfigKey]
supportedConfigKeys =
  [ Opt.command "hostname" $
      Opt.info (Opt.helper <*> hostnameParser) $
        Opt.progDesc "The hostname for the Cachix Deploy service."
  ]

hostnameParser :: Opt.Parser ConfigKey
hostnameParser = do
  hostname <-
    Opt.strArgument $
      mconcat
        [ Opt.metavar "HOSTNAME",
          Opt.showDefault
        ]
  pure (HostName hostname)

data Config = Config
  { authToken :: Token,
    hostName :: Text,
    binaryCaches :: [BinaryCacheConfig]
  }
  deriving (Show, Generic, Dhall.FromDhall, Dhall.ToDhall)

data BinaryCacheConfig = BinaryCacheConfig
  { name :: Text,
    secretKey :: Text
  }
  deriving (Show, Generic, Dhall.FromDhall, Dhall.ToDhall)

mkConfig :: Text -> Config
mkConfig token =
  Config
    { authToken = Token (toS token),
      hostName = "https://cachix.org",
      binaryCaches = []
    }

type ConfigPath = FilePath

readConfig :: ConfigPath -> IO (Maybe Config)
readConfig filename = do
  doesExist <- doesFileExist filename
  if doesExist
    then Just <$> Dhall.inputFile Dhall.auto filename
    else return Nothing

getDefaultFilename :: IO FilePath
getDefaultFilename = do
  dir <- getXdgDirectory XdgConfig "cachix"
  return $ dir <> "/cachix.dhall"

writeConfig :: ConfigPath -> Config -> IO ()
writeConfig filename config = do
  let doc = Dhall.Pretty.prettyExpr $ Dhall.embed Dhall.inject config
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
