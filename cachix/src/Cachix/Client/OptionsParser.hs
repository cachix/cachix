module Cachix.Client.OptionsParser
  ( CachixCommand (..),
    DaemonCommand (..),
    DaemonOptions (..),
    PushArguments (..),

    -- * Push options
    PushOptions (..),
    defaultPushOptions,
    defaultCompressionMethod,
    defaultCompressionLevel,
    defaultNumConcurrentChunks,
    defaultChunkSize,
    defaultNumJobs,
    defaultOmitDeriver,

    -- * Pin options
    PinOptions (..),

    -- * Global options
    Flags (..),

    -- * Misc
    BinaryCacheName,
    getOpts,
  )
where

import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.InstallationMode as InstallationMode
import Cachix.Client.URI (URI)
import qualified Cachix.Client.URI as URI
import qualified Cachix.Deploy.OptionsParser as DeployOptions
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Cachix.Types.BinaryCache as BinaryCache
import Cachix.Types.PinCreate (Keep (..))
import Data.Conduit.ByteString (ChunkSize)
import qualified Data.Text as T
import Options.Applicative
import Protolude hiding (toS)
import Protolude.Conv
import qualified Prelude

data Flags = Flags
  { configPath :: Config.ConfigPath,
    hostname :: Maybe URI,
    verbose :: Bool
  }

data CachixCommand
  = AuthToken (Maybe Text)
  | Config Config.Command
  | Daemon DaemonCommand
  | GenerateKeypair BinaryCacheName
  | Push PushArguments
  | Import PushOptions Text URI
  | Pin PinOptions
  | WatchStore PushOptions Text
  | WatchExec PushOptions Text Text [Text]
  | Use BinaryCacheName InstallationMode.UseOptions
  | Remove BinaryCacheName
  | DeployCommand DeployOptions.DeployCommand
  | Version
  deriving (Show)

data PushArguments
  = PushPaths PushOptions Text [Text]
  | PushWatchStore PushOptions Text
  deriving (Show)

data PinOptions = PinOptions
  { pinCacheName :: BinaryCacheName,
    pinName :: Text,
    pinStorePath :: Text,
    pinArtifacts :: [Text],
    pinKeep :: Maybe Keep
  }
  deriving (Show)

data PushOptions = PushOptions
  { -- | The compression level to use.
    compressionLevel :: Int,
    -- | The compression method to use.
    -- Default value taken rom the cache settings.
    compressionMethod :: Maybe BinaryCache.CompressionMethod,
    -- | The size of each uploaded part.
    --
    -- Common values for S3 are powers of 2 (8MiB, 16MiB, and so on), but this doesn't appear to be a requirement for S3 (unlike Glacier).
    -- The range is from 5MiB to 5GiB.
    --
    -- Lower values will increase HTTP overhead, higher values require more memory to preload each part.
    chunkSize :: Int,
    -- | The number of chunks to upload concurrently.
    -- The total memory usage is numJobs * numConcurrentChunks * chunkSize
    numConcurrentChunks :: Int,
    -- | The number of store paths to process concurrently.
    numJobs :: Int,
    -- | Omit the derivation from the store path metadata.
    omitDeriver :: Bool
  }
  deriving (Show)

defaultCompressionLevel :: Int
defaultCompressionLevel = 2

defaultCompressionMethod :: BinaryCache.CompressionMethod
defaultCompressionMethod = BinaryCache.ZSTD

defaultNumConcurrentChunks :: Int
defaultNumConcurrentChunks = 4

defaultChunkSize :: ChunkSize
defaultChunkSize = 32 * 1024 * 1024 -- 32MiB

minChunkSize :: ChunkSize
minChunkSize = 5 * 1024 * 1024 -- 5MiB

maxChunkSize :: ChunkSize
maxChunkSize = 5 * 1024 * 1024 * 1024 -- 5GiB

defaultNumJobs :: Int
defaultNumJobs = 8

defaultOmitDeriver :: Bool
defaultOmitDeriver = False

defaultPushOptions :: PushOptions
defaultPushOptions =
  PushOptions
    { compressionLevel = defaultCompressionLevel,
      compressionMethod = Nothing,
      chunkSize = defaultChunkSize,
      numConcurrentChunks = defaultNumConcurrentChunks,
      numJobs = defaultNumJobs,
      omitDeriver = defaultOmitDeriver
    }

data DaemonCommand
  = DaemonPushPaths DaemonOptions [FilePath]
  | DaemonRun DaemonOptions PushOptions BinaryCacheName
  | DaemonStop DaemonOptions
  | DaemonWatchExec PushOptions BinaryCacheName Text [Text]
  deriving (Show)

data DaemonOptions = DaemonOptions
  { daemonSocketPath :: Maybe FilePath
  }
  deriving (Show)

-- | CLI parser entry point
getOpts :: IO (Flags, CachixCommand)
getOpts = do
  configpath <- Config.getDefaultFilename
  let preferences = showHelpOnError <> showHelpOnEmpty <> helpShowGlobals <> subparserInline
  customExecParser (prefs preferences) (optsInfo configpath)

optsInfo :: Config.ConfigPath -> ParserInfo (Flags, CachixCommand)
optsInfo configpath =
  infoH parser $
    fullDesc
      <> progDesc "To get started, log in to https://app.cachix.org"
      <> header "https://cachix.org command line interface"
  where
    parser = (,) <$> flagParser configpath <*> (commandParser <|> versionParser)

commandParser :: Parser CachixCommand
commandParser =
  configCommands
    <|> cacheCommands
    <|> pushCommands
    <|> storePathCommands
    <|> daemonCommands
    <|> deployCommands
  where
    configCommands =
      subparser $
        fold
          [ commandGroup "Config commands:",
            hidden,
            command "authtoken" $ infoH authTokenCommand $ progDesc "Configure an authentication token for Cachix",
            command "config" configCommand
          ]

    cacheCommands =
      subparser $
        fold
          [ commandGroup "Cache commands:",
            hidden,
            command "generate-keypair" $ infoH generateKeypairCommand $ progDesc "Generate a signing key pair for a binary cache",
            command "use" $ infoH useCommand $ progDesc "Configure a binary cache in nix.conf",
            command "remove" $ infoH removeCommand $ progDesc "Remove a binary cache from nix.conf"
          ]

    pushCommands =
      subparser $
        fold
          [ commandGroup "Push commands:",
            command "push" $ infoH pushCommand $ progDesc "Upload Nix store paths to a binary cache",
            command "watch-exec" $ infoH watchExecCommand $ progDesc "Run a command while watching /nix/store for newly added store paths and upload them to a binary cache",
            command "watch-store" $ infoH watchStoreCommand $ progDesc "Watch /nix/store for newly added store paths and upload them to a binary cache",
            command "import" $ infoH importCommand $ progDesc "Import the contents of a binary cache from an S3-compatible object storage service into Cachix"
          ]

    storePathCommands =
      subparser $
        fold
          [ commandGroup "Store path commands:",
            hidden,
            command "pin" $ infoH pinCommand $ progDesc "Pin a store path to prevent it from being garbage collected"
          ]

    daemonCommands =
      subparser $
        fold
          [ commandGroup "Daemon commands:",
            hidden,
            command "daemon" $ infoH daemonCommand $ progDesc "Run a daemon that listens to push requests over a unix socket"
          ]

    deployCommands =
      subparser $
        fold
          [ commandGroup "Cachix Deploy commands:",
            hidden,
            command "deploy" $ infoH deployCommand $ progDesc "Manage remote Nix-based systems with Cachix Deploy"
          ]

flagParser :: Config.ConfigPath -> Parser Flags
flagParser defaultConfigPath =
  Flags <$> configPath <*> (host <|> hostname) <*> verbose
  where
    defaultHostname = URI.serialize URI.defaultCachixURI

    hostOpts =
      [ metavar "URI",
        help $ "Host to connect to (default: " <> defaultHostname <> ")"
      ]

    -- Accept both hostname and host.
    -- TODO: switch to host.
    hostname =
      optional . option uriOption $
        long "hostname" <> mconcat hostOpts

    host =
      optional . option uriOption $
        long "host" <> metavar "URI"

    configPath =
      strOption $
        mconcat
          [ long "config",
            short 'c',
            value defaultConfigPath,
            metavar "CONFIGPATH",
            showDefault,
            help "Cachix configuration file"
          ]

    verbose =
      switch $
        long "verbose"
          <> short 'v'
          <> help "Verbose mode"

uriOption :: ReadM URI
uriOption = eitherReader (first show . URI.parseURI . toS)

cacheNameParser :: Parser BinaryCacheName
cacheNameParser = strArgument (metavar "CACHE-NAME")

authTokenCommand :: Parser CachixCommand
authTokenCommand = AuthToken <$> (stdinFlag <|> (Just <$> authTokenArg))
  where
    stdinFlag = flag' Nothing (long "stdin" <> help "Read the auth token from stdin")
    authTokenArg = strArgument (metavar "AUTH-TOKEN")

configCommand :: ParserInfo CachixCommand
configCommand = Config <$> Config.parser

generateKeypairCommand :: Parser CachixCommand
generateKeypairCommand = GenerateKeypair <$> cacheNameParser

pushOptionsParser :: Parser PushOptions
pushOptionsParser =
  PushOptions
    <$> compressionLevel
    <*> compressionMethod
    <*> chunkSize
    <*> numConcurrentChunks
    <*> numJobs
    <*> omitDeriver
  where
    compressionLevel =
      option (auto >>= validateCompressionLevel) $
        long "compression-level"
          <> short 'c'
          <> metavar "[0..16]"
          <> help
            "The compression level to use. Supported range: [0-9] for xz and [0-16] for zstd."
          <> showDefault
          <> value defaultCompressionLevel

    validateCompressionLevel l =
      l
        <$ unless
          (l `elem` [0 .. 16])
          (readerError $ "value " <> show l <> " not in expected range: [0..16]")

    compressionMethod =
      option (eitherReader validateCompressionMethod) $
        long "compression-method"
          <> short 'm'
          <> metavar "xz | zstd"
          <> help
            "The compression method to use. Overrides the preferred compression method advertised by the cache. Supported methods: xz | zstd. Defaults to zstd."
          <> value Nothing

    validateCompressionMethod :: Prelude.String -> Either Prelude.String (Maybe BinaryCache.CompressionMethod)
    validateCompressionMethod method =
      if method `elem` ["xz", "zstd"]
        then case readEither (T.toUpper (toS method)) of
          Right a -> Right $ Just a
          Left b -> Left $ toS b
        else Left $ "Compression method " <> show method <> " not expected. Use xz or zstd."

    chunkSize =
      option (auto >>= validateChunkSize) $
        long "chunk-size"
          <> short 's'
          <> metavar prettyChunkRange
          <> help "The size of each uploaded part in bytes. The supported range is from 5MiB to 5GiB."
          <> showDefault
          <> value defaultChunkSize

    validateChunkSize c =
      c
        <$ unless
          (c >= minChunkSize && c <= maxChunkSize)
          (readerError $ "value " <> show c <> " not in expected range: " <> prettyChunkRange)

    prettyChunkRange = "[" <> show minChunkSize <> ".." <> show maxChunkSize <> "]"

    numConcurrentChunks =
      option auto $
        long "num-concurrent-chunks"
          <> short 'n'
          <> metavar "INT"
          <> help "The number of chunks to upload concurrently. The total memory usage is jobs * num-concurrent-chunks * chunk-size."
          <> showDefault
          <> value defaultNumConcurrentChunks

    numJobs =
      option auto $
        long "jobs"
          <> short 'j'
          <> metavar "INT"
          <> help "The number of threads to use when pushing store paths."
          <> showDefault
          <> value defaultNumJobs

    omitDeriver =
      switch $
        long "omit-deriver"
          <> help "Do not publish which derivations built the store paths."

pushCommand :: Parser CachixCommand
pushCommand = toPushCommand <$> pushOptionsParser <*> cacheNameParser <*> pushArgumentsParser
  where
    -- Figure out which subcommand we're in at the end and pass it the previous options.
    -- We can't do that beforehand because the paths argument parser needs to come after the cache name argument parser.
    toPushCommand pushOptions cacheName pushArguments =
      Push $ pushArguments pushOptions cacheName

pushArgumentsParser :: Parser (PushOptions -> BinaryCacheName -> PushArguments)
pushArgumentsParser = pushPathsParser <|> deprecatedPushWatchStoreParser

pushPathsParser :: Parser (PushOptions -> BinaryCacheName -> PushArguments)
pushPathsParser = toPushPaths <$> pathArgs
  where
    -- Move the path arguments to the end
    toPushPaths paths pushOptions cacheName =
      PushPaths pushOptions cacheName paths

    pathArgs =
      many $ strArgument (metavar "PATHS...")

deprecatedPushWatchStoreParser :: Parser (PushOptions -> BinaryCacheName -> PushArguments)
deprecatedPushWatchStoreParser =
  flag' PushWatchStore $
    long "watch-store"
      <> short 'w'
      <> help "DEPRECATED: use watch-store command instead."

importCommand :: Parser CachixCommand
importCommand =
  Import
    <$> pushOptionsParser
    <*> cacheNameParser
    <*> s3UriOption
  where
    s3UriOption =
      strArgument $
        metavar "S3-URI"
          <> help "e.g. s3://mybucket?endpoint=https://myexample.com&region=eu-central-1"

pinCommand :: Parser CachixCommand
pinCommand = Pin <$> pinOptionsParser

pinOptionsParser :: Parser PinOptions
pinOptionsParser =
  PinOptions
    <$> cacheNameParser
    <*> pinName
    <*> storePath
    <*> artifacts
    <*> keepParser
  where
    pinName = strArgument $ metavar "PIN-NAME"

    storePath = strArgument $ metavar "STORE-PATH"

    artifacts =
      many . strOption $
        metavar "ARTIFACTS..."
          <> long "artifact"
          <> short 'a'

keepParser :: Parser (Maybe Keep)
keepParser =
  daysParser
    <|> revisionsParser
    <|> foreverParser
    <|> pure Nothing
  where
    -- these three flag are mutually exclusive
    daysParser =
      Just . Days <$> keepDays

    keepDays =
      option auto $
        long "keep-days"
          <> metavar "INT"

    revisionsParser =
      Just . Revisions <$> keepRevisions

    keepRevisions =
      option auto $
        long "keep-revisions"
          <> metavar "INT"

    foreverParser =
      flag' (Just Forever) $
        long "keep-forever"

daemonCommand :: Parser CachixCommand
daemonCommand = Daemon <$> daemonSubCommand

daemonSubCommand :: Parser DaemonCommand
daemonSubCommand =
  subparser $
    fold
      [ command "push" $ infoH daemonPush $ progDesc "Push store paths to the daemon",
        command "run" $ infoH daemonRun $ progDesc "Launch the daemon",
        command "stop" $ infoH daemonStop $ progDesc "Stop the daemon and wait for any queued paths to be pushed",
        command "watch-exec" $ infoH daemonWatchExec $ progDesc "Run a command and upload any store paths built during its execution"
      ]

daemonPush :: Parser DaemonCommand
daemonPush =
  DaemonPushPaths
    <$> daemonOptionsParser
    <*> many (strArgument (metavar "PATHS..."))

daemonRun :: Parser DaemonCommand
daemonRun =
  DaemonRun
    <$> daemonOptionsParser
    <*> pushOptionsParser
    <*> cacheNameParser

daemonStop :: Parser DaemonCommand
daemonStop = DaemonStop <$> daemonOptionsParser

daemonWatchExec :: Parser DaemonCommand
daemonWatchExec =
  DaemonWatchExec
    <$> pushOptionsParser
    <*> cacheNameParser
    <*> strArgument (metavar "CMD")
    <*> many (strArgument (metavar "-- ARGS"))

daemonOptionsParser :: Parser DaemonOptions
daemonOptionsParser =
  DaemonOptions <$> socketOption
  where
    socketOption =
      optional . strOption $
        long "socket"
          <> short 's'
          <> metavar "SOCKET"

deployCommand :: Parser CachixCommand
deployCommand = DeployCommand <$> DeployOptions.parser

watchExecCommand :: Parser CachixCommand
watchExecCommand =
  WatchExec
    <$> pushOptionsParser
    <*> cacheNameParser
    <*> strArgument (metavar "CMD")
    <*> many (strArgument (metavar "-- ARGS"))

watchStoreCommand :: Parser CachixCommand
watchStoreCommand = WatchStore <$> pushOptionsParser <*> cacheNameParser

removeCommand :: Parser CachixCommand
removeCommand = Remove <$> cacheNameParser

useCommand :: Parser CachixCommand
useCommand = Use <$> cacheNameParser <*> installationMode

installationMode :: Parser InstallationMode.UseOptions
installationMode =
  InstallationMode.UseOptions <$> useMode <*> nixosFolderPath <*> outputDir
  where
    useMode =
      optional . option (maybeReader InstallationMode.fromString) $
        long "mode"
          <> short 'm'
          <> metavar "nixos | root-nixconf | user-nixconf"
          <> help "Mode in which to configure binary caches for Nix. Supported values: nixos | root-nixconf | user-nixconf"

    nixosFolderPath =
      strOption $
        long "nixos-folder"
          <> short 'd'
          <> help "Base directory for NixOS configuration generation"
          <> value "/etc/nixos/"
          <> showDefault

    outputDir =
      optional . strOption $
        long "output-directory"
          <> short 'O'
          <> help "Output directory where nix.conf and netrc will be updated."

versionParser :: Parser CachixCommand
versionParser =
  flag' Version $
    long "version"
      <> short 'V'
      <> help "Show cachix version"

-- TODO: usage footer
infoH :: Parser a -> InfoMod a -> ParserInfo a
infoH a = info (helper <*> a)
