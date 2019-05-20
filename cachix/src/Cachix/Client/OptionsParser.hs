module Cachix.Client.OptionsParser
  ( CachixCommand(..)
  , CachixOptions(..)
  , UseOptions(..)
  , BinaryCacheName
  , getOpts
  ) where

import Data.Bifunctor            (first)
import Protolude hiding          (option)
import URI.ByteString            (URIRef, Absolute, parseURI, strictURIParserOptions
                                 , serializeURIRef')
import Options.Applicative

import qualified Cachix.Client.Config as Config
import Cachix.Client.URI         (defaultCachixURI)
import qualified Cachix.Client.Commands.Export.OptionsParser as Export

data CachixOptions = CachixOptions
  { host :: URIRef Absolute
  , configPath :: Config.ConfigPath
  , verbose :: Bool
  } deriving Show

parserCachixOptions :: Config.ConfigPath -> Parser CachixOptions
parserCachixOptions defaultConfigPath = CachixOptions
  <$> option uriOption ( long "host"
                       <> short 'h'
                       <> value defaultCachixURI
                       <> metavar "URI"
                       <> showDefaultWith (toS . serializeURIRef')
                       <> help "Host to connect to"
                       )
 <*> strOption ( long "config"
              <> short 'c'
              <> value defaultConfigPath
              <> metavar "CONFIGPATH"
              <> showDefault
              <> help "Cachix configuration file"
              )
  <*> switch ( long "verbose"
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
  | Push BinaryCacheName [Text] Bool -- TODO: refactor to a record
  | Use BinaryCacheName UseOptions
  | Export Export.Arguments
  | Version
  deriving Show

data UseOptions = UseOptions
  { useNixOS :: Bool
  , useNixOSFolder :: FilePath
  } deriving Show

parserCachixCommand :: Parser CachixCommand
parserCachixCommand = subparser $
  command "authtoken" (infoH authtoken (progDesc "Configure token for authentication to cachix.org")) <>
  command "create" (infoH create (progDesc "DEPRECATED: Go to https://cachix.org instead")) <>
  command "export" (infoH (Export <$> Export.parseArguments) (progDesc "Read cachix secrets from local configuration and export in JSON Lines format")) <>
  command "generate-keypair" (infoH generateKeypair (progDesc "Generate keypair for an binary cache")) <>
  command "push" (infoH push (progDesc "Upload Nix store paths to the binary cache")) <>
  command "use" (infoH use (progDesc "Configure nix.conf to enable binary cache during builds"))
  where
    nameArg = strArgument (metavar "NAME")
    authtoken = AuthToken <$> strArgument (metavar "TOKEN")
    create = Create <$> nameArg
    generateKeypair = GenerateKeypair <$> nameArg
    push = Push <$> nameArg
                <*> many (strArgument (metavar "PATHS..."))
                <*> switch ( long "watch-store"
                          <> short 'w'
                          <> help "Run in daemon mode and push store paths as they are added to /nix/store"
                           )
    use = Use <$> nameArg
              <*> (UseOptions <$> switch ( long "nixos"
                                        <> short 'n'
                                        <> help "Write NixOS modules")
                              <*> strOption ( long "nixos-folder"
                                           <> short 'd'
                                           <> help "Base directory for NixOS configuration"
                                           <> value "/etc/nixos/"
                                           <> showDefault
                                           ))

getOpts :: IO (CachixOptions, CachixCommand)
getOpts = do
  configpath <- Config.getDefaultFilename
  customExecParser (prefs showHelpOnEmpty) (opts configpath)

opts :: Config.ConfigPath -> ParserInfo (CachixOptions, CachixCommand)
opts configpath = infoH parser desc
  where parser = (,) <$> parserCachixOptions configpath <*> (parserCachixCommand <|> versionParser)
        versionParser :: Parser CachixCommand
        versionParser = flag' Version ( long "version"
                     <> short 'V'
                     <> help "Show cachix version"
                     )

desc :: InfoMod a
desc = fullDesc
    <> progDesc "Sign into https://cachix.org to get started."
    <> header "cachix.org command interface"
    -- TODO: usage footer

infoH :: Parser a -> InfoMod a -> ParserInfo a
infoH a = info (helper <*> a)
