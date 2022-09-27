{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.Config
  ( -- CLI options
    run,
    parser,
    Command (..),
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
import Cachix.Client.URI (URI)
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

data CachixOptions = CachixOptions
  { host :: URI,
    configPath :: ConfigPath,
    verbose :: Bool
  }
  deriving (Show)

data Config = Config
  { authToken :: Token,
    hostname :: URI,
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

-- CLI parsers

data Command
  = Get GetCommand
  | Set SetCommand
  deriving (Show)

data GetCommand
  = GetHostname
  deriving (Show)

data SetCommand
  = SetHostname URI
  deriving (Show)

run :: CachixOptions -> Command -> IO ()
run CachixOptions {configPath} (Get cmd) = getConfigOption configPath cmd
run CachixOptions {configPath} (Set cmd) = setConfigOption configPath cmd

getConfigOption :: ConfigPath -> GetCommand -> IO ()
getConfigOption configPath cmd = do
  config <- getConfig configPath
  case cmd of
    GetHostname -> putStrLn (URI.serialize (hostname config) :: Text)

setConfigOption :: ConfigPath -> SetCommand -> IO ()
setConfigOption configPath (SetHostname hostname) = do
  config <- getConfig configPath
  writeConfig configPath config {hostname}

parser :: Opt.ParserInfo Command
parser =
  Opt.info (Opt.helper <*> commandParser) $
    Opt.progDesc "Manage configuration settings for cachix"

commandParser :: Opt.Parser Command
commandParser =
  Opt.subparser $
    mconcat
      [ Opt.command "get" $
          Opt.info (Opt.helper <*> getConfigOptionParser) $
            Opt.progDesc "Get a configuration option",
        Opt.command "set" $
          Opt.info (Opt.helper <*> setConfigOptionParser) $
            Opt.progDesc "Set a configuration option"
      ]

getConfigOptionParser :: Opt.Parser Command
getConfigOptionParser =
  Opt.subparser $ Opt.metavar "KEY" <> mconcat (supportedOptions False)

setConfigOptionParser :: Opt.Parser Command
setConfigOptionParser =
  Opt.subparser $ Opt.metavar "KEY VALUE" <> mconcat (supportedOptions True)

supportedOptions :: Bool -> [Opt.Mod Opt.CommandFields Command]
supportedOptions canModify =
  [ Opt.command "hostname" $
      Opt.info (Opt.helper <*> if canModify then fmap Set hostnameParser else pure (Get GetHostname)) $
        Opt.progDesc "The hostname for the Cachix Deploy service."
  ]

hostnameParser :: Opt.Parser SetCommand
hostnameParser = do
  SetHostname
    <$> Opt.argument uriOption (Opt.metavar "HOSTNAME")

uriOption :: Opt.ReadM URI
uriOption = Opt.eitherReader $ \s ->
  first show $ URI.parseURI (toS s)
