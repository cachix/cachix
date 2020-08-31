module Cachix.Client.OptionsParser
  ( CachixCommand (..),
    CachixOptions (..),
    PushArguments (..),
    PushOptions (..),
    BinaryCacheName,
    getOpts,
  )
where

import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.InstallationMode as InstallationMode
import Cachix.Client.URI (defaultCachixURI)
import Options.Applicative
import Protolude hiding (option, toS)
import Protolude.Conv
import URI.ByteString
  ( Absolute,
    URIRef,
    parseURI,
    serializeURIRef',
    strictURIParserOptions,
  )

data CachixOptions
  = CachixOptions
      { host :: URIRef Absolute,
        configPath :: Config.ConfigPath,
        verbose :: Bool
      }
  deriving (Show)

parserCachixOptions :: Config.ConfigPath -> Parser CachixOptions
parserCachixOptions defaultConfigPath =
  CachixOptions
    <$> option
      uriOption
      ( long "host"
          <> value defaultCachixURI
          <> metavar "URI"
          <> showDefaultWith (toS . serializeURIRef')
          <> help "Host to connect to"
      )
    <*> strOption
      ( long "config"
          <> short 'c'
          <> value defaultConfigPath
          <> metavar "CONFIGPATH"
          <> showDefault
          <> help "Cachix configuration file"
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose mode"
      )

uriOption :: ReadM (URIRef Absolute)
uriOption = eitherReader $ \s ->
  first show $ parseURI strictURIParserOptions $ toS s

type BinaryCacheName = Text

data CachixCommand
  = AuthToken Text
  | Create BinaryCacheName
  | GenerateKeypair BinaryCacheName
  | Push PushArguments
  | Use BinaryCacheName InstallationMode.UseOptions
  | Version
  deriving (Show)

data PushArguments
  = PushPaths PushOptions Text [Text]
  | PushWatchStore PushOptions Text
  deriving (Show)

data PushOptions
  = PushOptions
      { compressionLevel :: Int,
        numJobs :: Int,
        omitDeriver :: Bool
      }
  deriving (Show)

parserCachixCommand :: Parser CachixCommand
parserCachixCommand =
  subparser $
    command "authtoken" (infoH authtoken (progDesc "Configure authentication token for communication to cachix.org API"))
      <> command "create" (infoH create (progDesc "DEPRECATED: Go to https://cachix.org instead"))
      <> command "generate-keypair" (infoH generateKeypair (progDesc "Generate keypair for a binary cache"))
      <> command "push" (infoH push (progDesc "Upload Nix store paths to a binary cache"))
      <> command "use" (infoH use (progDesc "Configure a binary cache by writing nix.conf and netrc files."))
  where
    nameArg = strArgument (metavar "CACHE-NAME")
    authtoken = AuthToken <$> strArgument (metavar "AUTHTOKEN")
    create = Create <$> nameArg
    generateKeypair = GenerateKeypair <$> nameArg
    validatedLevel l =
      l <$ unless (l `elem` [0 .. 9]) (readerError $ "value " <> show l <> " not in expected range: [0..9]")
    pushOptions :: Parser PushOptions
    pushOptions =
      PushOptions
        <$> option
          (auto >>= validatedLevel)
          ( long "compression-level"
              <> short 'c'
              <> metavar "[0..9]"
              <> help
                "The compression level for XZ compression.\
                \ Take compressor *and* decompressor memory usage into account before using [7..9]!"
              <> showDefault
              <> value 2
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
    pushWatchStore =
      (\() opts cache -> PushWatchStore opts cache)
        <$> flag'
          ()
          ( long "watch-store"
              <> short 'w'
              <> help "Run in daemon mode and push store paths as they are added to /nix/store"
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

getOpts :: IO (CachixOptions, CachixCommand)
getOpts = do
  configpath <- Config.getDefaultFilename
  customExecParser (prefs showHelpOnEmpty) (optsInfo configpath)

optsInfo :: Config.ConfigPath -> ParserInfo (CachixOptions, CachixCommand)
optsInfo configpath = infoH parser desc
  where
    parser = (,) <$> parserCachixOptions configpath <*> (parserCachixCommand <|> versionParser)
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
    <> progDesc "Sign into https://cachix.org to get started."
    <> header "cachix.org command interface"

-- TODO: usage footer
infoH :: Parser a -> InfoMod a -> ParserInfo a
infoH a = info (helper <*> a)
