{-# LANGUAGE DeriveFunctor #-}

{- (Very limited) parser, renderer and modifier of nix.conf

Supports subset of nix.conf given Nix 2.0 or Nix 1.0

When reading config files, it normalizes Nix 1.0/2.0 names to unified naming,
then when it writes the config back, it uses naming depending what given Nix
version considers as recommended.

-}
module Cachix.Client.NixConf
  ( NixConf,
    NixConfG (..),
    NixConfLine (..),
    NixConfSource,
    NixConfSourceG (..),
    NixConfLoc (..),
    IncludeType (..),
    new,
    render,
    add,
    remove,
    read,
    readWithDefault,
    resolveIncludes,
    write,
    getFilename,
    parser,
    parse,
    readLines,
    writeLines,
    isTrustedUsers,
    defaultPublicURI,
    defaultSigningKey,
  )
where

import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.URI qualified as URI
import Cachix.Types.BinaryCache qualified as BinaryCache
import Data.List (nub)
import Data.Text qualified as T
import Protolude hiding (toS)
import Protolude.Conv (toS)
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
  )
import System.FilePath (normalise)
import System.FilePath.Posix (takeDirectory, (</>))
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char

defaultPublicURI :: Text
defaultPublicURI = "https://cache.nixos.org"

defaultSigningKey :: Text
defaultSigningKey = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="

data NixConfLine
  = Substituters [Text]
  | TrustedUsers [Text]
  | TrustedPublicKeys [Text]
  | NetRcFile Text
  | Include IncludeType
  | Other Text
  deriving (Show, Eq)

data IncludeType
  = RequiredInclude Text -- for "include"
  | OptionalInclude Text -- for "!include"
  deriving (Show, Eq)

-- | A list of conf lines
type NixConf = NixConfG [NixConfLine]

newtype NixConfG a = NixConf a
  deriving stock (Show, Eq, Functor)

-- | A wrapper around NixConf that also tracks the path to the nix.conf file
type NixConfSource = NixConfSourceG NixConf

data NixConfSourceG a = NixConfSource
  { nixConfPath :: FilePath,
    nixConfLines :: a
  }
  deriving stock (Show, Eq, Functor)

-- | Operations on nix.conf.
-- Helps to work with both NixConf and NixConfSource.
class NixConfOps a where
  -- | Read the nix.conf lines that match the given predicate
  readLines :: (NixConfLine -> Maybe [Text]) -> a -> [Text]

  -- | Write the given lines to the nix.conf
  writeLines :: (NixConfLine -> Maybe [Text]) -> NixConfLine -> a -> a

  -- | Add the given binary cache to the nix.conf
  add :: BinaryCache.BinaryCache -> [a] -> a -> a

  -- | Remove the given binary cache from the nix.conf
  remove :: URI.URI -> Text -> [a] -> a -> (a, Bool)

  -- | Render the nix.conf to a Text
  render :: a -> Text

instance NixConfOps NixConf where
  readLines predicate (NixConf xs) = foldl f [] xs
    where
      f :: [Text] -> NixConfLine -> [Text]
      f prev next = prev <> fromMaybe [] (predicate next)

  writeLines predicate addition = fmap f
    where
      f x = filter (isNothing . predicate) x <> [addition]

  add bc toRead toWrite =
    writeLines isPublicKey (TrustedPublicKeys $ nub publicKeys) $
      writeLines isSubstituter (Substituters $ nub substituters) toWrite
    where
      -- Note: some defaults are always appended since overriding some setttings in nix.conf overrides defaults otherwise
      substituters = (defaultPublicURI : concatMap (readLines isSubstituter) toRead) <> [BinaryCache.uri bc]
      publicKeys = (defaultSigningKey : concatMap (readLines isPublicKey) toRead) <> BinaryCache.publicSigningKeys bc

  remove uri name toRead toWrite =
    (newconf, oldsubstituters /= substituters)
    where
      newconf =
        writeLines isPublicKey (TrustedPublicKeys $ nub publicKeys) $
          writeLines isSubstituter (Substituters $ nub substituters) toWrite
      oldsubstituters = concatMap (readLines isSubstituter) toRead
      substituters = filter (toS (URI.toByteString fulluri) /=) oldsubstituters
      oldpublicKeys = concatMap (readLines isPublicKey) toRead
      publicKeys = filter (not . T.isPrefixOf (toS $ URI.hostBS $ URI.getHostname fulluri)) oldpublicKeys
      fulluri = URI.appendSubdomain name uri

  render (NixConf ls) = T.unlines $ fmap go ls
    where
      go :: NixConfLine -> Text
      go (Substituters xs) = "substituters" <> " = " <> T.unwords xs
      go (TrustedUsers xs) = "trusted-users = " <> T.unwords xs
      go (TrustedPublicKeys xs) = "trusted-public-keys" <> " = " <> T.unwords xs
      go (NetRcFile filename) = "netrc-file = " <> filename
      go (Include (RequiredInclude path)) = "include " <> path
      go (Include (OptionalInclude path)) = "!include " <> path
      go (Other line) = line

instance NixConfOps NixConfSource where
  readLines f = readLines f . nixConfLines
  writeLines f = fmap . writeLines f
  add bc toRead toWrite = toWrite {nixConfLines = add bc (fmap nixConfLines toRead) (nixConfLines toWrite)}
  remove uri name toRead toWrite =
    let (newLines, changed) = remove uri name (fmap nixConfLines toRead) (nixConfLines toWrite)
     in (toWrite {nixConfLines = newLines}, changed)
  render = render . nixConfLines

isSubstituter :: NixConfLine -> Maybe [Text]
isSubstituter (Substituters xs) = Just xs
isSubstituter _ = Nothing

isPublicKey :: NixConfLine -> Maybe [Text]
isPublicKey (TrustedPublicKeys xs) = Just xs
isPublicKey _ = Nothing

isTrustedUsers :: NixConfLine -> Maybe [Text]
isTrustedUsers (TrustedUsers xs) = Just xs
isTrustedUsers _ = Nothing

-- | Create a new, empty NixConfSource with the given path
new :: FilePath -> NixConfSource
new path = NixConfSource path (NixConf [])

write :: NixConfSource -> IO ()
write NixConfSource {nixConfPath, nixConfLines} = do
  createDirectoryIfMissing True (takeDirectory nixConfPath)
  writeFile nixConfPath $ render nixConfLines

-- | Resolves includes in the given NixConfSource, starting from the given source file.
resolveIncludes :: NixConfSource -> IO [NixConfSource]
resolveIncludes conf@NixConfSource {nixConfPath} =
  resolveIncludesWithStack [normalise nixConfPath] nixConfPath conf

resolveIncludesWithStack :: [FilePath] -> FilePath -> NixConfSource -> IO [NixConfSource]
resolveIncludesWithStack stack baseFile baseConf@(NixConfSource {nixConfLines = NixConf ls}) = do
  includedConfigs <- mapM resolveInclude [f | Include f <- ls]
  return $ baseConf : concat includedConfigs
  where
    dir = takeDirectory baseFile

    resolveInclude :: IncludeType -> IO [NixConfSource]
    resolveInclude includeType = do
      let path = case includeType of
            RequiredInclude p -> p
            OptionalInclude p -> p
          fullPath = normalise $ dir </> toS path

      if fullPath `elem` stack
        then case includeType of
          RequiredInclude _ -> throwIO $ CircularInclude (formatCircularError fullPath)
          OptionalInclude _ -> return []
        else do
          exists <- doesFileExist fullPath
          if not exists && isRequired includeType
            then throwIO $ IncludeNotFound $ toS fullPath
            else do
              content <- readFile fullPath
              case parse content of
                Left _err -> return []
                Right conf -> resolveIncludesWithStack (fullPath : stack) baseFile (NixConfSource fullPath conf)

    isRequired (RequiredInclude _) = True
    isRequired (OptionalInclude _) = False

    formatCircularError path =
      "Circular include detected:\n" <> T.intercalate "\n" (formatChain (reverse stack) path)

    formatChain :: [FilePath] -> FilePath -> [Text]
    formatChain chain target =
      case chain of
        [] -> []
        (p : ps) ->
          format p
            : map (("    -> includes " <>) . format) ps
            ++ ["    -> includes " <> format target <> " (circular reference)\n"]
      where
        format = toS . normalise :: FilePath -> Text

data NixConfLoc = Global | Local | Custom FilePath
  deriving stock (Show, Eq)

read :: NixConfLoc -> IO (Maybe NixConfSource)
read ncl = do
  filename <- getFilename ncl
  read' filename

read' :: FilePath -> IO (Maybe NixConfSource)
read' filename = do
  doesExist <- doesFileExist filename
  if not doesExist
    then return Nothing
    else do
      result <- parse <$> readFile filename
      case result of
        Left err -> do
          putStrLn (Mega.errorBundlePretty err)
          panic $ toS filename <> " failed to parse, please copy the above error and contents of nix.conf and open an issue at https://github.com/cachix/cachix"
        Right conf -> return (Just (NixConfSource filename conf))

readWithDefault :: NixConfLoc -> IO NixConfSource
readWithDefault ncl = do
  filename <- getFilename ncl
  fromMaybe (new filename) <$> read' filename

getFilename :: NixConfLoc -> IO FilePath
getFilename ncl = do
  dir <-
    case ncl of
      Global -> return "/etc/nix"
      Local -> getXdgDirectory XdgConfig "nix"
      Custom filepath -> return filepath
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

parseInclude :: (Text -> IncludeType) -> Text -> Parser NixConfLine
parseInclude constr name = Mega.try $ do
  _ <- optional (some (char ' '))
  _ <- string name
  _ <- some (char ' ')
  path <- many (Mega.satisfy (not . isSpace))
  _ <- many spaceChar
  return $ Include (constr (toS path))

parseOther :: Parser NixConfLine
parseOther = Mega.try $ Other . toS <$> Mega.someTill Mega.anySingle (void eol <|> Mega.eof)

parseAltLine :: Parser NixConfLine
parseAltLine =
  (Other "" <$ eol)
    <|> parseLine Substituters "substituters"
    <|> parseLine TrustedPublicKeys "trusted-public-keys"
    <|> parseLine TrustedUsers "trusted-users"
    <|> parseLine TrustedPublicKeys "binary-cache-public-keys"
    <|> parseLine Substituters "binary-caches"
    -- NB: assume that space in this option means space in filename
    <|> parseLine (NetRcFile . T.concat) "netrc-file"
    <|> parseInclude RequiredInclude "include"
    <|> parseInclude OptionalInclude "!include"
    <|> parseOther

parser :: Parser NixConf
parser = NixConf <$> many parseAltLine

parse :: Text -> Either (Mega.ParseErrorBundle Text Void) NixConf
parse = Mega.parse parser "nix.conf"
