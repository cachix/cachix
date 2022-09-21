{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.Config
  ( -- CLI options
    run,
    parser,
    CachixOptions (..),
    Command (..),
    ConfigKey (..),
    -- Auth token helpers
    getAuthTokenRequired,
    getAuthTokenOptional,
    getAuthTokenMaybe,
    setAuthToken,
    noAuthTokenError,
    -- Config
    getConfig,
    readConfig,
    writeConfig,
    getDefaultFilename,
    ConfigPath,
    Config (..),
    BinaryCacheConfig (..),
    setBinaryCaches,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.URI as URI
import qualified Control.Exception.Safe as Safe
import Data.Either.Extra (eitherToMaybe)
import Data.String.Here
import qualified Dhall
import qualified Dhall.Pretty
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as Opt.Help
import qualified Prettyprinter as Pretty
import qualified Prettyprinter.Render.Text as Pretty
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
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
import qualified URI.ByteString as URI

data CachixOptions = CachixOptions
  { host :: URI.URIRef URI.Absolute,
    configPath :: ConfigPath,
    verbose :: Bool
  }
  deriving (Show)

data Command
  = Set ConfigKey
  deriving (Show)

data ConfigKey
  = HostName (URI.URIRef URI.Absolute)
  deriving (Show)

run :: CachixOptions -> Command -> IO ()
run cachixOptions (Set option) = setConfigOption cachixOptions option

setConfigOption :: CachixOptions -> ConfigKey -> IO ()
setConfigOption CachixOptions {configPath} (HostName hostname) = do
  config <- getConfig configPath
  writeConfig configPath config {hostName = hostname}

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
    Opt.argument
      uriOption
      $ mconcat
        [ Opt.metavar "HOSTNAME"
        ]
  pure (HostName hostname)

uriOption :: Opt.ReadM (URI.URIRef URI.Absolute)
uriOption = Opt.eitherReader $ \s ->
  first show $ URI.parseURI URI.strictURIParserOptions (toS s)

data Config = Config
  { authToken :: Token,
    hostName :: URI.URIRef URI.Absolute,
    binaryCaches :: [BinaryCacheConfig]
  }
  deriving (Show, Generic, Dhall.ToDhall, Dhall.FromDhall)

data BinaryCacheConfig = BinaryCacheConfig
  { name :: Text,
    secretKey :: Text
  }
  deriving (Show, Generic, Dhall.FromDhall, Dhall.ToDhall)

defaultConfig :: Config
defaultConfig =
  Config
    { authToken = "",
      hostName = URI.defaultCachixURI,
      binaryCaches = []
    }

mergeWithDefault :: Text -> Text
mergeWithDefault config =
  serializeConfig defaultConfig <> " // " <> config

type ConfigPath = FilePath

getConfig :: ConfigPath -> IO Config
getConfig filename = do
  userConfig <- readConfig filename
  pure $ fromMaybe defaultConfig userConfig

readConfig :: ConfigPath -> IO (Maybe Config)
readConfig filename = fmap eitherToMaybe . Safe.tryIO $ do
  userConfig <- readFile filename
  let config = mergeWithDefault userConfig
  Dhall.detailed (Dhall.input Dhall.auto config)

getDefaultFilename :: IO FilePath
getDefaultFilename = do
  dir <- getXdgDirectory XdgConfig "cachix"
  return $ dir <> "/cachix.dhall"

serializeConfig :: Config -> Text
serializeConfig =
  Pretty.renderStrict
    . Pretty.layoutPretty Pretty.defaultLayoutOptions
    . Dhall.Pretty.prettyExpr
    . Dhall.embed Dhall.inject

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

getAuthTokenRequired :: Config -> IO Token
getAuthTokenRequired config = do
  authTokenMaybe <- getAuthTokenMaybe config
  case authTokenMaybe of
    Just authtoken -> return authtoken
    Nothing -> throwIO $ NoConfig $ toS noAuthTokenError

-- TODO: https://github.com/haskell-servant/servant-auth/issues/173
getAuthTokenOptional :: Config -> IO Token
getAuthTokenOptional = pure . authToken

-- get auth token from env variable or fallback to config
-- TODO: we could fetch this env variable when creating the config.
-- It would make all these options (and their precedence) much more obvious.
getAuthTokenMaybe :: Config -> IO (Maybe Token)
getAuthTokenMaybe config = do
  maybeAuthToken <- lookupEnv "CACHIX_AUTH_TOKEN"
  case maybeAuthToken of
    Just token -> return $ Just $ Token (toS token)
    Nothing -> return $ Just (authToken config)

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

-- Setters

setAuthToken :: Token -> Config -> Config
setAuthToken token config = config {authToken = token}

setBinaryCaches :: [BinaryCacheConfig] -> Config -> Config
setBinaryCaches caches config = config {binaryCaches = binaryCaches config <> caches}
