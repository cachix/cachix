{- (Very limited) parser, rendered and modifier of nix.conf

Supports subset of nix.conf given Nix 2.0 or Nix 1.0

When reading config files, it normalizes Nix 1.0/2.0 names to unified naming,
then when it writes the config back, it uses naming depending what given Nix
version considers as recommended.

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
  , substitutersKey
  , trustedPublicKeysKey
  ) where

import Data.Char (isSpace)
import Data.Text (unwords, unlines)
import Data.List (nub)
import Protolude
import qualified Text.Megaparsec as Mega
import Text.Megaparsec.Char
import System.Directory ( doesFileExist, createDirectoryIfMissing
                        , getXdgDirectory, XdgDirectory(..)
                        )
import System.FilePath.Posix ( takeDirectory )
import Cachix.Api (BinaryCache(..))
import Cachix.Client.NixVersion (NixVersion(..))


data NixConfLine
  = Substituters [Text]
  | TrustedUsers [Text]
  | TrustedPublicKeys [Text]
  | Other Text
  deriving (Show, Eq)

newtype NixConfG a = NixConf a
  deriving (Show, Eq, Functor)

type NixConf = NixConfG [NixConfLine]

readLines :: [NixConf] -> (NixConfLine -> Maybe [Text]) -> [Text]
readLines nixconfs predicate = concatMap f nixconfs
  where
    f (NixConf xs) = foldl foldIt [] xs

    foldIt :: [Text] -> NixConfLine -> [Text]
    foldIt prev new = prev <> fromMaybe [] (predicate new)

writeLines :: (NixConfLine -> Maybe [Text]) -> NixConfLine -> NixConf -> NixConf
writeLines predicate addition = fmap f
  where
    f x = filter (isNothing . predicate) x <> [addition]

isSubstituter :: NixConfLine -> Maybe [Text]
isSubstituter (Substituters xs) = Just xs
isSubstituter _ = Nothing

isPublicKey :: NixConfLine -> Maybe [Text]
isPublicKey (TrustedPublicKeys xs) = Just xs
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

render :: NixVersion -> NixConf -> Text
render nixversion (NixConf nixconflines) = unlines $ fmap go nixconflines
  where
    go :: NixConfLine -> Text
    go (Substituters xs) = substitutersKey nixversion <> " = " <> unwords xs
    go (TrustedUsers xs) = "trusted-users = " <> unwords xs
    go (TrustedPublicKeys xs) = trustedPublicKeysKey nixversion <> " = " <> unwords xs
    go (Other line) = line

substitutersKey :: NixVersion -> Text
substitutersKey Nix1XX = "binary-caches"
substitutersKey Nix20 = "substituters"
substitutersKey Nix201 = "substituters"

trustedPublicKeysKey :: NixVersion -> Text
trustedPublicKeysKey Nix1XX = "binary-cache-public-keys"
trustedPublicKeysKey Nix20 = "trusted-public-keys"
trustedPublicKeysKey Nix201 = "trusted-public-keys"

write :: NixVersion -> NixConfLoc -> NixConf -> IO ()
write nixversion ncl nc = do
  filename <- getFilename ncl
  createDirectoryIfMissing True (takeDirectory filename)
  writeFile filename $ render nixversion nc

read :: NixConfLoc -> IO (Maybe NixConf)
read ncl = do
  filename <- getFilename ncl
  doesExist <- doesFileExist filename
  if not doesExist
  then return Nothing
  else do
    result <- parse <$> readFile filename
    case result of
      Left err -> do
        putStrLn (Mega.errorBundlePretty err)
        panic $ toS filename <> " failed to parse, please copy the above error and contents of nix.conf and open an issue at https://github.com/cachix/cachix"
      Right conf -> return $ Just conf

update :: NixVersion -> NixConfLoc -> (Maybe NixConf -> NixConf) -> IO ()
update nixversion ncl f = do
  nc <- f <$> read ncl
  write nixversion ncl nc

data NixConfLoc = Global | Local
  deriving (Show, Eq)

getFilename :: NixConfLoc -> IO FilePath
getFilename ncl = do
  dir <- case ncl of
    Global -> return "/etc/nix"
    Local -> getXdgDirectory XdgConfig "nix"
  return $ dir <> "/nix.conf"

-- nix.conf Parser

type Parser = Mega.Parsec Void Text

-- TODO: handle comments
parseLine :: ([Text] -> NixConfLine) -> Text -> Parser NixConfLine
parseLine constr name = Mega.try $ do
    _ <- optional (some (char ' '))
    _ <- string name
    _ <- many (char ' ')
    _ <- char '='
    _ <- many (char ' ')
    values <- Mega.sepBy1 (many (Mega.satisfy (not . isSpace))) (some (char ' '))
    _ <- many spaceChar
    return $ constr (fmap toS values)

parseOther :: Parser NixConfLine
parseOther = Mega.try $ Other . toS <$> Mega.someTill Mega.anySingle (void eol <|> Mega.eof)

parseAltLine :: Parser NixConfLine
parseAltLine =
      (const (Other "")  <$> eol)
  <|> parseLine Substituters "substituters"
  <|> parseLine TrustedPublicKeys "trusted-public-keys"
  <|> parseLine TrustedUsers "trusted-users"
  <|> parseLine TrustedPublicKeys "binary-cache-public-keys"
  <|> parseLine Substituters "binary-caches"
  <|> parseOther

parser :: Parser NixConf
parser = NixConf <$> many parseAltLine

parse :: Text -> Either (Mega.ParseErrorBundle Text Void) NixConf
parse = Mega.parse parser "nix.conf"
