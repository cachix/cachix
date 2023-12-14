{-# LANGUAGE ApplicativeDo #-}

module Cachix.Client.OptionsParser
  ( CachixCommand (..),
    DaemonCommand (..),
    DaemonOptions (..),
    PushArguments (..),
    PushOptions (..),
    PinOptions (..),
    BinaryCacheName,
    getOpts,
    Flags (..),
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

flagParser :: Config.ConfigPath -> Parser Flags
flagParser defaultConfigPath = do
  hostname <-
    optional $
      option
        uriOption
        ( mconcat
            [ long "hostname",
              metavar "URI",
              help ("Host to connect to (default: " <> defaultHostname <> ")")
            ]
        )
        -- Accept `host` for backwards compatibility
        <|> option uriOption (long "host" <> hidden)

  configPath <-
    strOption $
      mconcat
        [ long "config",
          short 'c',
          value defaultConfigPath,
          metavar "CONFIGPATH",
          showDefault,
          help "Cachix configuration file"
        ]

  verbose <-
    switch $
      mconcat
        [ long "verbose",
          short 'v',
          help "Verbose mode"
        ]

  pure Flags {hostname, configPath, verbose}
  where
    defaultHostname = URI.serialize URI.defaultCachixURI

uriOption :: ReadM URI
uriOption = eitherReader $ \s ->
  first show $ URI.parseURI (toS s)

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
  { compressionLevel :: Int,
    compressionMethod :: Maybe BinaryCache.CompressionMethod,
    numJobs :: Int,
    omitDeriver :: Bool
  }
  deriving (Show)

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

commandParser :: Parser CachixCommand
commandParser =
  subparser $
    command "authtoken" (infoH authtoken (progDesc "Configure an authentication token for Cachix"))
      <> command "config" (Config <$> Config.parser)
      <> (hidden <> command "daemon" (infoH (Daemon <$> daemon) (progDesc "Run a daemon that listens to push requests over a unix socket")))
      <> command "generate-keypair" (infoH generateKeypair (progDesc "Generate a signing key pair for a binary cache"))
      <> command "push" (infoH push (progDesc "Upload Nix store paths to a binary cache"))
      <> command "import" (infoH import' (progDesc "Import the contents of a binary cache from an S3-compatible object storage service into Cachix"))
      <> command "pin" (infoH pin (progDesc "Pin a store path to prevent it from being garbage collected"))
      <> command "watch-exec" (infoH watchExec (progDesc "Run a command while watching /nix/store for newly added store paths and upload them to a binary cache"))
      <> command "watch-store" (infoH watchStore (progDesc "Watch /nix/store for newly added store paths and upload them to a binary cache"))
      <> command "use" (infoH use (progDesc "Configure a binary cache in nix.conf"))
      <> command "remove" (infoH remove (progDesc "Remove a binary cache from nix.conf"))
      <> command "deploy" (infoH (DeployCommand <$> DeployOptions.parser) (progDesc "Manage remote Nix-based systems with Cachix Deploy"))
  where
    nameArg = strArgument (metavar "CACHE-NAME")
    authtoken = AuthToken <$> (stdinFlag <|> (Just <$> authTokenArg))
      where
        stdinFlag = flag' Nothing (long "stdin" <> help "Read the auth token from stdin")
        authTokenArg = strArgument (metavar "AUTH-TOKEN")
    generateKeypair = GenerateKeypair <$> nameArg
    validatedLevel l =
      l <$ unless (l `elem` [0 .. 16]) (readerError $ "value " <> show l <> " not in expected range: [0..16]")
    validatedMethod :: Prelude.String -> Either Prelude.String (Maybe BinaryCache.CompressionMethod)
    validatedMethod method =
      if method `elem` ["xz", "zstd"]
        then case readEither (T.toUpper (toS method)) of
          Right a -> Right $ Just a
          Left b -> Left $ toS b
        else Left $ "Compression method " <> show method <> " not expected. Use xz or zstd."
    pushOptions :: Parser PushOptions
    pushOptions =
      PushOptions
        <$> option
          (auto >>= validatedLevel)
          ( long "compression-level"
              <> short 'c'
              <> metavar "[0..16]"
              <> help
                "The compression level to use. Supported range: [0-9] for xz and [0-16] for zstd."
              <> showDefault
              <> value 2
          )
        <*> option
          (eitherReader validatedMethod)
          ( long "compression-method"
              <> short 'm'
              <> metavar "xz | zstd"
              <> help
                "The compression method to use. Supported methods: xz | zstd. Defaults to zstd."
              <> value Nothing
          )
        <*> option
          auto
          ( long "jobs"
              <> short 'j'
              <> help "The number of threads to use when pushing store paths."
              <> showDefault
              <> value 8
          )
        <*> switch (long "omit-deriver" <> help "Do not publish which derivations built the store paths.")
    push = (\opts cache f -> Push $ f opts cache) <$> pushOptions <*> nameArg <*> (pushPaths <|> pushWatchStore)
    pushPaths =
      (\paths opts cache -> PushPaths opts cache paths)
        <$> many (strArgument (metavar "PATHS..."))
    import' = Import <$> pushOptions <*> nameArg <*> strArgument (metavar "S3-URI" <> help "e.g. s3://mybucket?endpoint=https://myexample.com&region=eu-central-1")
    keepParser = daysParser <|> revisionsParser <|> foreverParser <|> pure Nothing
    -- these three flag are mutually exclusive
    daysParser = Just . Days <$> option auto (long "keep-days" <> metavar "INT")
    revisionsParser = Just . Revisions <$> option auto (long "keep-revisions" <> metavar "INT")
    foreverParser = flag' (Just Forever) (long "keep-forever")
    pinOptions =
      PinOptions
        <$> nameArg
        <*> strArgument (metavar "PIN-NAME")
        <*> strArgument (metavar "STORE-PATH")
        <*> many (strOption (metavar "ARTIFACTS..." <> long "artifact" <> short 'a'))
        <*> keepParser
    pin = Pin <$> pinOptions
    daemon =
      subparser $
        command "push" (infoH daemonPush (progDesc "Push store paths to the daemon"))
          <> command "run" (infoH daemonRun (progDesc "Launch the daemon"))
          <> command "stop" (infoH daemonStop (progDesc "Stop the daemon and wait for any queued paths to be pushed"))
          <> command "watch-exec" (infoH daemonWatchExec (progDesc "Run a command and upload any store paths built during its execution"))
    daemonPush = DaemonPushPaths <$> daemonOptions <*> many (strArgument (metavar "PATHS..."))
    daemonRun = DaemonRun <$> daemonOptions <*> pushOptions <*> nameArg
    daemonStop = DaemonStop <$> daemonOptions
    daemonWatchExec = DaemonWatchExec <$> pushOptions <*> nameArg <*> strArgument (metavar "CMD") <*> many (strArgument (metavar "-- ARGS"))
    daemonOptions = DaemonOptions <$> optional (strOption (long "socket" <> short 's' <> metavar "SOCKET"))
    watchExec = WatchExec <$> pushOptions <*> nameArg <*> strArgument (metavar "CMD") <*> many (strArgument (metavar "-- ARGS"))
    watchStore = WatchStore <$> pushOptions <*> nameArg
    pushWatchStore =
      (\() opts cache -> PushWatchStore opts cache)
        <$> flag'
          ()
          ( long "watch-store"
              <> short 'w'
              <> help "DEPRECATED: use watch-store command instead."
          )
    remove = Remove <$> nameArg
    use =
      Use
        <$> nameArg
        <*> ( InstallationMode.UseOptions
                <$> optional
                  ( option
                      (maybeReader InstallationMode.fromString)
                      ( long "mode"
                          <> short 'm'
                          <> metavar "nixos | root-nixconf | user-nixconf"
                          <> help "Mode in which to configure binary caches for Nix. Supported values: nixos | root-nixconf | user-nixconf"
                      )
                  )
                <*> strOption
                  ( long "nixos-folder"
                      <> short 'd'
                      <> help "Base directory for NixOS configuration generation"
                      <> value "/etc/nixos/"
                      <> showDefault
                  )
                <*> optional
                  ( strOption
                      ( long "output-directory"
                          <> short 'O'
                          <> help "Output directory where nix.conf and netrc will be updated."
                      )
                  )
            )

getOpts :: IO (Flags, CachixCommand)
getOpts = do
  configpath <- Config.getDefaultFilename
  let preferences = showHelpOnError <> showHelpOnEmpty <> helpShowGlobals <> subparserInline
  customExecParser (prefs preferences) (optsInfo configpath)

optsInfo :: Config.ConfigPath -> ParserInfo (Flags, CachixCommand)
optsInfo configpath = infoH parser desc
  where
    parser = (,) <$> flagParser configpath <*> (commandParser <|> versionParser)
    versionParser :: Parser CachixCommand
    versionParser =
      flag'
        Version
        ( long "version"
            <> short 'V'
            <> help "Show cachix version"
        )

desc :: InfoMod a
desc =
  fullDesc
    <> progDesc "To get started log in to https://app.cachix.org"
    <> header "https://cachix.org command line interface"

-- TODO: usage footer
infoH :: Parser a -> InfoMod a -> ParserInfo a
infoH a = info (helper <*> a)
