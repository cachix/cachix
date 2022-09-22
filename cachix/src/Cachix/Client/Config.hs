{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.Config
  ( -- CLI options
    run,
    parser,
    Command (..),
    Option (..),
    CachixOptions (..),
    -- Auth token helpers
    getAuthTokenRequired,
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
  = Set Option
  deriving (Show)

data Option
  = HostName (URI.URIRef URI.Absolute)
  deriving (Show)

run :: CachixOptions -> Command -> IO ()
run cachixoptions (Set option) = setConfigOption cachixoptions option

setConfigOption :: CachixOptions -> Option -> IO ()
setConfigOption CachixOptions {configPath} (HostName hostname) = do
  config <- getConfig configPath
  writeConfig configPath config {hostname = hostname}

parser :: Opt.ParserInfo Command
parser =
  Opt.info (Opt.helper <*> commandParser) $
    Opt.progDesc "Manage the configuration for cachix"

commandParser :: Opt.Parser Command
commandParser =
  configOptionParser Set

configOptionParser :: (Option -> Command) -> Opt.Parser Command
configOptionParser cmd = do
  cmd
    <$> Opt.subparser (Opt.metavar "KEY VALUE" <> mconcat supportedOptions)

supportedOptions :: [Opt.Mod Opt.CommandFields Option]
supportedOptions =
  [ Opt.command "hostname" $
      Opt.info (Opt.helper <*> hostnameParser) $
        Opt.progDesc "The hostname for the Cachix Deploy service."
  ]

hostnameParser :: Opt.Parser Option
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
    hostname :: URI.URIRef URI.Absolute,
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
    { authToken = Token "",
      hostname = URI.defaultCachixURI,
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

getAuthTokenFromConfig :: Config -> Maybe Token
getAuthTokenFromConfig = inspectToken . authToken
  where
    inspectToken (Token "") = Nothing
    inspectToken token = Just token

getAuthTokenRequired :: Config -> IO Token
getAuthTokenRequired config = do
  authTokenMaybe <- getAuthTokenMaybe config
  case authTokenMaybe of
    Just authtoken -> return authtoken
    Nothing -> throwIO $ NoConfig $ toS noAuthTokenError

-- get auth token from env variable or fallback to config
getAuthTokenMaybe :: Config -> IO (Maybe Token)
getAuthTokenMaybe config = do
  maybeAuthToken <- lookupEnv "CACHIX_AUTH_TOKEN"
  case maybeAuthToken of
    Just token -> return $ Just $ Token (toS token)
    Nothing -> return $ getAuthTokenFromConfig config

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
