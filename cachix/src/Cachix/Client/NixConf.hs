module Cachix.Client.NixConf
  ( NixConf(..)
  , NixConfLine(..)
  , NixConfLoc(..)
  , render
  , add
  , read
  , update
  , parser
  , parse
  ) where

import Protolude hiding (some, many)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Data.Text
import Data.List (nub)
import System.Directory ( doesFileExist, createDirectoryIfMissing
                        , getXdgDirectory, XdgDirectory(..)
                        )


data NixConfLine
  = Substituters [Text]
  | TrustedUsers [Text]
  | TrustedPublicKeys [Text]
  | Other Text
  deriving (Show, Eq)

newtype NixConf = NixConf [NixConfLine] deriving (Show, Eq)


add :: NixConfLine -> Text -> NixConfLine
add (Substituters xs) x = Substituters $ nub (x : xs)
add (TrustedPublicKeys xs) x = TrustedPublicKeys $ nub (x : xs)
add rest _ = rest

render :: NixConf -> Text
render (NixConf nixconflines) = unlines $ fmap go nixconflines
  where
    go :: NixConfLine -> Text
    go (Substituters xs) = "substituters = " <> unwords xs
    go (TrustedUsers xs) = "trusted-users = " <> unwords xs
    go (TrustedPublicKeys xs) = "trusted-public-keys = " <> unwords xs
    go (Other line) = line

write :: NixConfLoc -> NixConf -> IO ()
write ncl nc = do
  filename <- getFilename ncl
  writeFile filename $ render nc

read :: NixConfLoc -> IO (Maybe NixConf)
read ncl = do
  filename <- getFilename ncl
  doesExist <- doesFileExist filename
  if doesExist
  then parse <$> readFile filename
  else return Nothing

update :: NixConfLoc -> (Maybe NixConf -> NixConf) -> IO ()
update ncl f = do
  nc <- f <$> read ncl
  write ncl nc

data NixConfLoc = Global | Local

getFilename :: NixConfLoc -> IO FilePath
getFilename ncl = do
  dir <- case ncl of
    Global -> return "/etc/nix"
    Local -> getXdgDirectory XdgConfig "nix"
  createDirectoryIfMissing True dir
  return $ dir <> "/nix.conf"

-- nix.conf Parser

type Parser = Parsec Void Text

-- TODO: handle comments
parseLine :: ([Text] -> NixConfLine) -> Text -> Parser NixConfLine
parseLine constr name = do
    _ <- optional (some (char ' '))
    _ <- string name
    _ <- many (char ' ')
    _ <- char '='
    _ <- many (char ' ')
    values <- sepBy1 (many alphaNumChar) (some (char ' '))
    _ <- many spaceChar
    return $ constr (fmap toS values)

parseOther :: Parser NixConfLine
parseOther = Other . toS <$> manyTill anyChar eol

parseAltLine :: Parser NixConfLine
parseAltLine =
      parseLine Substituters "substituters"
  <|> parseLine TrustedPublicKeys "trusted-public-keys"
  <|> parseLine TrustedUsers "trusted-users"
  <|> parseOther

parser :: Parser NixConf
parser = NixConf <$> many parseAltLine

parse :: Text -> Maybe NixConf
parse = parseMaybe parser
