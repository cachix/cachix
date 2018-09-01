{-# LANGUAGE QuasiQuotes #-}
module Cachix.Client.OptionsParser
  ( CachixCommand(..)
  , CachixOptions(..)
  , BinaryCacheName
  , getOpts
  ) where

import Data.Bifunctor            (first)
import Protolude hiding          (option)
import URI.ByteString            (URIRef, Absolute, parseURI, strictURIParserOptions
                                 , serializeURIRef')
import URI.ByteString.QQ
import Options.Applicative

import qualified Cachix.Client.Config as Config

data CachixOptions = CachixOptions
  { host :: URIRef Absolute
  , configPath :: Config.ConfigPath
  , verbose :: Bool
  } deriving Show

parserCachixOptions :: Config.ConfigPath -> Parser CachixOptions
parserCachixOptions configpath = CachixOptions
  <$> option uriOption ( long "host"
                       <> short 'h'
                       <> value [uri|https://cachix.org|]
                       <> metavar "URI"
                       <> showDefaultWith (toS . serializeURIRef')
                       <> help "Host to connect to"
                       )
 <*> strOption ( long "config"
              <> short 'c'
              <> value configpath
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
  | Push BinaryCacheName [Text] Bool -- TODO: refactor to a record
  | Use BinaryCacheName Bool --- TODO: refactor to a record
  | Version
  deriving Show


parserCachixCommand :: Parser CachixCommand
parserCachixCommand = subparser $
  command "authtoken" (infoH authtoken (progDesc "Configure token for authentication to cachix.org")) <>
  command "create" (infoH create (progDesc "Create a new binary cache")) <>
  command "push" (infoH push (progDesc "Upload Nix store paths to the binary cache")) <>
  command "use" (infoH use (progDesc "Configure nix.conf to enable binary cache during builds"))
  where
    authtoken = AuthToken <$> strArgument (metavar "TOKEN")
    create = Create <$> strArgument (metavar "NAME")
    push = Push <$> strArgument (metavar "NAME")
                <*> many (strArgument (metavar "PATHS..."))
                <*> switch (long "watch-store" <> short 'w' <> help "Run in daemon mode and push store paths as they are added to /nix/store")
    use = Use <$> strArgument (metavar "NAME")
              <*> switch (long "nixos" <> short 'n' <> help "Output NixOS configuration lines")

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
