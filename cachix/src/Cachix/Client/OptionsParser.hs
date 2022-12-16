{-# LANGUAGE ApplicativeDo #-}

module Cachix.Client.OptionsParser
  ( CachixCommand (..),
    PushArguments (..),
    PushOptions (..),
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
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Data.Text as T
import Options.Applicative
import Protolude hiding (option, toS)
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

type BinaryCacheName = Text

data CachixCommand
  = AuthToken (Maybe Text)
  | Config Config.Command
  | GenerateKeypair BinaryCacheName
  | Push PushArguments
  | WatchStore PushOptions Text
  | WatchExec PushOptions Text Text [Text]
  | Use BinaryCacheName InstallationMode.UseOptions
  | DeployCommand DeployOptions.DeployCommand
  | Version
  deriving (Show)

data PushArguments
  = PushPaths PushOptions Text [Text]
  | PushWatchStore PushOptions Text
  deriving (Show)

data PushOptions = PushOptions
  { compressionLevel :: Int,
    compressionMethod :: Maybe BinaryCache.CompressionMethod,
    numJobs :: Int,
    omitDeriver :: Bool
  }
  deriving (Show)

commandParser :: Parser CachixCommand
commandParser =
  subparser $
    command "authtoken" (infoH authtoken (progDesc "Configure authentication token for communication to HTTP API"))
      <> command "config" (Config <$> Config.parser)
      <> command "generate-keypair" (infoH generateKeypair (progDesc "Generate signing key pair for a binary cache"))
      <> command "push" (infoH push (progDesc "Upload Nix store paths to a binary cache"))
      <> command "watch-exec" (infoH watchExec (progDesc "Run a command while it's running watch /nix/store for newly added store paths and upload them to a binary cache"))
      <> command "watch-store" (infoH watchStore (progDesc "Indefinitely watch /nix/store for newly added store paths and upload them to a binary cache"))
      <> command "use" (infoH use (progDesc "Configure a binary cache by writing nix.conf and netrc files"))
      <> command "deploy" (infoH (DeployCommand <$> DeployOptions.parser) (progDesc "Cachix Deploy commands"))
  where
    nameArg = strArgument (metavar "CACHE-NAME")
    authtoken = AuthToken <$> (stdinFlag <|> (Just <$> authTokenArg))
      where
        stdinFlag = flag' Nothing (long "stdin" <> help "Read the token from stdin rather than accepting it as an argument.")
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
                "The compression level for XZ compression between 0-9 and ZSTD 0-16."
              <> showDefault
              <> value 2
          )
        <*> option
          (eitherReader validatedMethod)
          ( long "compression-method"
              <> short 'm'
              <> metavar "xz | zstd"
              <> help
                "The compression method, either xz or zstd."
              <> value Nothing
          )
        <*> option
          auto
          ( long "jobs"
              <> short 'j'
              <> help "Number of threads used for pushing store paths."
              <> showDefault
              <> value 4
          )
        <*> switch (long "omit-deriver" <> help "Do not publish which derivations built the store paths.")
    push = (\opts cache f -> Push $ f opts cache) <$> pushOptions <*> nameArg <*> (pushPaths <|> pushWatchStore)
    pushPaths =
      (\paths opts cache -> PushPaths opts cache paths)
        <$> many (strArgument (metavar "PATHS..."))
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
    use =
      Use <$> nameArg
        <*> ( InstallationMode.UseOptions
                <$> optional
                  ( option
                      (maybeReader InstallationMode.fromString)
                      ( long "mode"
                          <> short 'm'
                          <> help "Mode in which to configure binary caches for Nix. Supported values: nixos, root-nixconf, user-nixconf"
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
  customExecParser (prefs showHelpOnEmpty) (optsInfo configpath)

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
