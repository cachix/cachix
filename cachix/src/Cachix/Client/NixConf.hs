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
    NixConfLoc (..),
    IncludeType (..),
    new,
    render,
    add,
    remove,
    read,
    readWithDefault,
    resolveIncludes,
    update,
    write,
    getFilename,
    parser,
    parse,
    readLines,
    writeLines,
    isTrustedUsers,
    defaultPublicURI,
    defaultSigningKey,
    setNetRC,
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

data IncludeType
  = RequiredInclude Text -- for "include"
  | OptionalInclude Text -- for "!include"
  deriving (Show, Eq)

data NixConfLine
  = Substituters [Text]
  | TrustedUsers [Text]
  | TrustedPublicKeys [Text]
  | NetRcFile Text
  | Include IncludeType
  | Other Text
  deriving (Show, Eq)

data NixConfG a = NixConf
  { nixConfPath :: FilePath,
    nixConfLines :: a
  }
  deriving stock (Show, Eq, Functor)

type NixConf = NixConfG [NixConfLine]

new :: FilePath -> NixConf
new path = NixConf path []

readLines :: [NixConf] -> (NixConfLine -> Maybe [Text]) -> [Text]
readLines nixconfs predicate = concatMap f nixconfs
  where
    f (NixConf {nixConfLines}) = foldl foldIt [] nixConfLines
    foldIt :: [Text] -> NixConfLine -> [Text]
    foldIt prev next = prev <> fromMaybe [] (predicate next)

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
add :: BinaryCache.BinaryCache -> [NixConf] -> NixConf -> NixConf
add bc toRead toWrite =
  writeLines isPublicKey (TrustedPublicKeys $ nub publicKeys) $
    writeLines isSubstituter (Substituters $ nub substituters) toWrite
  where
    -- Note: some defaults are always appended since overriding some setttings in nix.conf overrides defaults otherwise
    substituters = (defaultPublicURI : readLines toRead isSubstituter) <> [BinaryCache.uri bc]
    publicKeys = (defaultSigningKey : readLines toRead isPublicKey) <> BinaryCache.publicSigningKeys bc

remove :: URI.URI -> Text -> [NixConf] -> NixConf -> (NixConf, Bool)
remove uri name toRead toWrite =
  (newconf, oldsubstituters /= substituters)
  where
    newconf =
      writeLines isPublicKey (TrustedPublicKeys $ nub publicKeys) $
        writeLines isSubstituter (Substituters $ nub substituters) toWrite
    oldsubstituters = readLines toRead isSubstituter
    substituters = filter (toS (URI.toByteString fulluri) /=) oldsubstituters
    oldpublicKeys = readLines toRead isPublicKey
    publicKeys = filter (not . T.isPrefixOf (toS $ URI.hostBS $ URI.getHostname fulluri)) oldpublicKeys
    fulluri = URI.appendSubdomain name uri

render :: NixConf -> Text
render (NixConf {nixConfLines}) = T.unlines $ fmap go nixConfLines
  where
    go :: NixConfLine -> Text
    go (Substituters xs) = "substituters" <> " = " <> T.unwords xs
    go (TrustedUsers xs) = "trusted-users = " <> T.unwords xs
    go (TrustedPublicKeys xs) = "trusted-public-keys" <> " = " <> T.unwords xs
    go (NetRcFile filename) = "netrc-file = " <> filename
    go (Include (RequiredInclude path)) = "include " <> path
    go (Include (OptionalInclude path)) = "!include " <> path
    go (Other line) = line

write :: NixConfLoc -> NixConf -> IO ()
write ncl nc = do
  filename <- getFilename ncl
  createDirectoryIfMissing True (takeDirectory filename)
  writeFile filename $ render nc

-- | Resolves includes in the given NixConf, starting from the given source file.
resolveIncludes :: NixConf -> IO [NixConf]
resolveIncludes conf@NixConf {nixConfPath} = resolveIncludesWithStack [normalise nixConfPath] nixConfPath conf

resolveIncludesWithStack :: [FilePath] -> FilePath -> NixConf -> IO [NixConf]
resolveIncludesWithStack stack baseFile baseConf@(NixConf {nixConfLines}) = do
  includedConfigs <- mapM resolveInclude [f | Include f <- nixConfLines]
  return $ baseConf : concat includedConfigs
  where
    dir = takeDirectory baseFile

    resolveInclude :: IncludeType -> IO [NixConf]
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
                Right conf -> resolveIncludesWithStack (fullPath : stack) baseFile (NixConf fullPath conf)

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

read :: NixConfLoc -> IO (Maybe NixConf)
read ncl = do
  filename <- getFilename ncl
  read' filename

read' :: FilePath -> IO (Maybe NixConf)
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
        Right conf -> return (Just (NixConf filename conf))

readWithDefault :: NixConfLoc -> IO NixConf
readWithDefault ncl = do
  filename <- getFilename ncl
  fromMaybe (new filename) <$> read' filename

update :: NixConfLoc -> (Maybe NixConf -> NixConf) -> IO ()
update ncl f = do
  nc <- f <$> read ncl
  write ncl nc

setNetRC :: Text -> NixConf -> NixConf
setNetRC netrc conf = conf {nixConfLines = filter noNetRc (nixConfLines conf) ++ [NetRcFile netrc]}
  where
    noNetRc (NetRcFile _) = False
    noNetRc _ = True

data NixConfLoc = Global | Local | Custom FilePath
  deriving (Show, Eq)

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

parser :: Parser [NixConfLine]
parser = many parseAltLine

parse :: Text -> Either (Mega.ParseErrorBundle Text Void) [NixConfLine]
parse = Mega.parse parser "nix.conf"
