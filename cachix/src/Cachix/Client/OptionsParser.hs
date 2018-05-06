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
  (first show $ parseURI strictURIParserOptions $ toS s)

type BinaryCacheName = Text

data CachixCommand
  = AuthToken Text
  | Create BinaryCacheName
  | Sync BinaryCacheName
  | Use BinaryCacheName
  deriving Show

parserCachixCommand :: Parser CachixCommand
parserCachixCommand = subparser $
  command "authtoken" (infoH authtoken (progDesc "Print greeting")) <>
  command "create" (infoH create (progDesc "Say goodbye")) <>
  command "sync" (infoH sync (progDesc "Say goodbye")) <>
  command "use" (infoH use (progDesc "Say goodbye"))
  where
    authtoken = AuthToken <$> strArgument (metavar "TOKEN")
    create = Create <$> strArgument (metavar "NAME")
    sync = Sync <$> strArgument (metavar "NAME")
    use = Use <$> strArgument (metavar "NAME")

getOpts :: IO (CachixOptions, CachixCommand)
getOpts = customExecParser (prefs showHelpOnEmpty) opts

opts :: ParserInfo (CachixOptions, CachixCommand)
opts = infoH parser desc
  where parser = (,) <$> parserCachixOptions <*> parserCachixCommand

desc :: InfoMod a
desc = fullDesc
    <> progDesc "TODO"
    <> header "cachix.org command interface"
    -- TODO: usage footer

infoH :: Parser a -> InfoMod a -> ParserInfo a
infoH a = info (helper <*> a)
