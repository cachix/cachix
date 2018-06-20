{- (Very limited) parser, rendered and modifier of nix.conf
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module Cachix.Client.NixConf
  ( NixConf
  , NixConfG(..)
  , NixConfLine(..)
  , NixConfLoc(..)
  , render
  , add
  , read
  , update
  , write
  , getFilename
  , parser
  , parse
  , readLines
  , writeLines
  , isTrustedUsers
  , defaultPublicURI
  , defaultSigningKey
  ) where

import Control.Exception (catch)
import Data.Char (isSpace)
import Data.Text (unwords, unlines)
import Data.List (nub)
import Protolude hiding (some, many)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import System.Directory ( doesFileExist, createDirectoryIfMissing
                        , getXdgDirectory, XdgDirectory(..)
                        )
import Cachix.Api (BinaryCache(..))


data NixConfLine
  = Substituters [Text]
  | TrustedUsers [Text]
  | TrustedPublicKeys [Text]
  | BinaryCachePublicKeys [Text]
  | BinaryCaches [Text]
  | Other Text
  deriving (Show, Eq)

newtype NixConfG a = NixConf a
  deriving (Show, Eq, Functor)

type NixConf = NixConfG [NixConfLine]

readLines :: [NixConf] -> (NixConfLine -> Maybe [Text]) -> [Text]
readLines nixconfs predicate = concat $ fmap f nixconfs
  where
    f (NixConf xs) = foldl foldIt [] xs

    foldIt :: [Text] -> NixConfLine -> [Text]
    foldIt prev new = prev <> (fromMaybe [] $ predicate new)

writeLines :: (NixConfLine -> Maybe [Text]) -> NixConfLine -> NixConf -> NixConf
writeLines predicate addition nixconf = fmap f nixconf
  where
    f x = filter (isNothing . predicate) x <> [addition]

isSubstituter :: NixConfLine -> Maybe [Text]
isSubstituter (Substituters xs) = Just xs
isSubstituter (BinaryCaches xs) = Just xs
isSubstituter _ = Nothing

isPublicKey :: NixConfLine -> Maybe [Text]
isPublicKey (TrustedPublicKeys xs) = Just xs
isPublicKey (BinaryCachePublicKeys xs) = Just xs
isPublicKey _ = Nothing

isTrustedUsers :: NixConfLine -> Maybe [Text]
isTrustedUsers (TrustedUsers xs) = Just xs
isTrustedUsers _ = Nothing

-- | Pure version of addIO
add :: BinaryCache -> [NixConf] -> NixConf -> NixConf
add BinaryCache{..} toRead toWrite =
  writeLines isPublicKey (TrustedPublicKeys $ nub publicKeys) $
    writeLines isSubstituter (Substituters $ nub substituters) toWrite
  where
    -- Note: some defaults are always appended since overriding some setttings in nix.conf overrides defaults otherwise
    substituters = (defaultPublicURI : readLines toRead isSubstituter) <> [uri]
    publicKeys = (defaultSigningKey : readLines toRead isPublicKey) <> publicSigningKeys

defaultPublicURI :: Text
defaultPublicURI = "https://cache.nixos.org"
defaultSigningKey :: Text
defaultSigningKey = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="

render :: NixConf -> Text
render (NixConf nixconflines) = unlines $ fmap go nixconflines
  where
    go :: NixConfLine -> Text
    go (Substituters xs) = "substituters = " <> unwords xs
    go (BinaryCaches xs) = "substituters = " <> unwords xs
    go (TrustedUsers xs) = "trusted-users = " <> unwords xs
    go (TrustedPublicKeys xs) = "trusted-public-keys = " <> unwords xs
    go (BinaryCachePublicKeys xs) = "trusted-public-keys = " <> unwords xs
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
  deriving (Show, Eq)

getFilename :: NixConfLoc -> IO FilePath
getFilename ncl = do
  dir <- case ncl of
    Global -> return "/etc/nix"
    Local -> getXdgDirectory XdgConfig "nix"
  _ <- catch (createDirectoryIfMissing True dir) $ \e ->
    hPutStr stderr ("Warning: Couldn't create " <> dir <> " :" <> show (e :: IOException))
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
    values <- sepBy1 (many (satisfy (not . isSpace))) (some (char ' '))
    _ <- many spaceChar
    return $ constr (fmap toS values)

parseOther :: Parser NixConfLine
parseOther = Other . toS <$> manyTill anyChar eol

parseAltLine :: Parser NixConfLine
parseAltLine =
      parseLine Substituters "substituters"
  <|> parseLine TrustedPublicKeys "trusted-public-keys"
  <|> parseLine TrustedUsers "trusted-users"
  <|> parseLine BinaryCachePublicKeys "binary-cache-public-keys"
  <|> parseLine BinaryCaches "binary-caches"
  <|> parseOther

parser :: Parser NixConf
parser = NixConf <$> many parseAltLine

parse :: Text -> Maybe NixConf
parse = parseMaybe parser
