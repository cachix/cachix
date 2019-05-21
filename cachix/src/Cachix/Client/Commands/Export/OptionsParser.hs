module Cachix.Client.Commands.Export.OptionsParser 
  ( Output(..)
  , Arguments(..)
  , parseArguments
  ) where

import Protolude
import Options.Applicative
import System.FilePath (FilePath)

data Output = Stdout | Out FilePath | Append FilePath
  deriving Show

data Arguments = Arguments
  { outputFile :: Output
  , pushSecrets :: [Text]
  , pullSecrets :: [Text]
  } deriving Show

parseArguments :: Parser Arguments
parseArguments = Arguments
  <$> ( flag' Stdout ( long "stdout"
                        <> help "Write to stdout"
                         )
      <|> Out <$> strOption (long "write-to"
                            <> short 'o'
                            <> metavar "FILE"
                            <> help "Write to fresh or truncated FILE"
                            <> completer (bashCompleter "file")
                             )
      <|> Append <$> strOption (long "append-to"
                            <> short 'a'
                            <> metavar "FILE"
                            <> help "Write by appending to new or existing FILE"
                            <> completer (bashCompleter "file")
                             )
  )
  <*> many (strOption ( long "push"
          <> metavar "CACHE"
          <> help "Export config+secrets for pushing and pulling with CACHE"
           ))
  <*> many (strOption ( long "pull"
          <> metavar "CACHE"
          <> help "Export config+secrets for pulling from CACHE"
           ))
