module Cachix.Client.OptionsParser
  ( CachixCommand (..),
    CachixOptions (..),
    UseOptions (..),
    PushArguments (..),
    PushOptions (..),
    BinaryCacheName,
    getOpts
    )
where

import qualified Cachix.Client.Config as Config
import Cachix.Client.URI (defaultCachixURI)
import Data.Bifunctor (first)
import Options.Applicative
import Protolude hiding (option)
import URI.ByteString
  ( Absolute,
    URIRef,
    parseURI,
    serializeURIRef',
    strictURIParserOptions
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
    <$> option uriOption
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
  | Use BinaryCacheName UseOptions
  | Version
  deriving (Show)

data UseOptions
  = UseOptions
      { useNixOS :: Bool,
        useNixOSFolder :: FilePath
        }
  deriving (Show)

data PushArguments
  = PushPaths PushOptions Text [Text]
  | PushWatchStore PushOptions Text
  deriving (Show)

data PushOptions
  = PushOptions
      { compressionLevel :: Int
        }
  deriving (Show)

parserCachixCommand :: Parser CachixCommand
parserCachixCommand =
  subparser
    $ command "authtoken" (infoH authtoken (progDesc "Configure token for authentication to cachix.org"))
    <> command "create" (infoH create (progDesc "DEPRECATED: Go to https://cachix.org instead"))
    <> command "generate-keypair" (infoH generateKeypair (progDesc "Generate keypair for a binary cache"))
    <> command "push" (infoH push (progDesc "Upload Nix store paths to the binary cache"))
    <> command "use" (infoH use (progDesc "Configure nix.conf to enable binary cache during builds"))
  where
    nameArg = strArgument (metavar "NAME")
    authtoken = AuthToken <$> strArgument (metavar "TOKEN")
    create = Create <$> nameArg
    generateKeypair = GenerateKeypair <$> nameArg
    validatedLevel l =
      l <$ unless (l `elem` [0 .. 9]) (readerError $ "value " <> show l <> " not in expected range: [0..9]")
    pushOptions :: Parser PushOptions
    pushOptions =
      PushOptions
        <$> option (auto >>= validatedLevel)
              ( long "compression-level"
                  <> metavar "[0..9]"
                  <> help
                       "The compression level for XZ compression.\
                   \ Take compressor *and* decompressor memory usage into account before using [7..9]!"
                  <> showDefault
                  <> value 2
                )
    push = (\opts cache f -> Push $ f opts cache) <$> pushOptions <*> nameArg <*> (pushPaths <|> pushWatchStore)
    pushPaths =
      (\paths opts cache -> PushPaths opts cache paths)
        <$> many (strArgument (metavar "PATHS..."))
    pushWatchStore =
      (\() opts cache -> PushWatchStore opts cache)
        <$> flag' ()
              ( long "watch-store"
                  <> short 'w'
                  <> help "Run in daemon mode and push store paths as they are added to /nix/store"
                )
    use =
      Use <$> nameArg
        <*> ( UseOptions
                <$> switch
                      ( long "nixos"
                          <> short 'n'
                          <> help "Write NixOS modules"
                        )
                <*> strOption
                      ( long "nixos-folder"
                          <> short 'd'
                          <> help "Base directory for NixOS configuration"
                          <> value "/etc/nixos/"
                          <> showDefault
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
      flag' Version
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
