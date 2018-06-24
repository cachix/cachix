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


data CachixOptions = CachixOptions
  { host :: URIRef Absolute
  , verbose :: Bool
  } deriving Show

parserCachixOptions :: Parser CachixOptions
parserCachixOptions = CachixOptions
  <$> option uriOption ( long "host"
                       <> short 'h'
                       <> value [uri|https://cachix.org|]
                       <> metavar "URI"
                       <> showDefaultWith (toS . serializeURIRef')
                       <> help "Host to connect to"
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
getOpts = customExecParser (prefs showHelpOnEmpty) opts

opts :: ParserInfo (CachixOptions, CachixCommand)
opts = infoH parser desc
  where parser = (,) <$> parserCachixOptions <*> (parserCachixCommand <|> versionParser)
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
