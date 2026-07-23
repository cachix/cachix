{-# LANGUAGE DeriveFunctor #-}

{- (Very limited) parser, renderer and modifier of nix.conf

Only the settings cachix manages are rewritten. Lines with inline '#' comments
and Nix 1.0 alias keys (binary-caches, binary-cache-public-keys) are parsed
for READING only: their values (as Nix sees them, with everything from '#' to
the end of the line stripped) are visible to the trust and substituter checks,
but the lines carry their raw text and render back unchanged, so a rewrite
never alters lines cachix does not own.

-}
module Cachix.Client.NixConf
  ( NixConf,
    NixConfG (..),
    NixConfLine (..),
    NixConfSource,
    NixConfSourceG (..),
    NixConfLoc (..),
    IncludeType (..),
    MigrateLegacy (..),
    new,
    render,
    addCache,
    removeCache,
    addCacheStandalone,
    removeCacheStandalone,
    legacyCaches,
    ensureOptionalInclude,
    isSubstituter,
    cachixConf,
    read,
    readWithDefault,
    readPathWithDefault,
    readPathQuiet,
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
import Control.Exception.Safe qualified as Safe
import Data.List (nub)
import Data.Text qualified as T
import Protolude hiding (toS)
import Protolude.Conv (toS)
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    getXdgDirectory,
  )
import System.FilePath (normalise)
import System.FilePath.Posix (takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import System.IO.Error qualified
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char

defaultPublicURI :: Text
defaultPublicURI = "https://cache.nixos.org"

defaultSigningKey :: Text
defaultSigningKey = "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="

data NixConfLine
  = Substituters [Text]
  | ExtraSubstituters [Text]
  | TrustedUsers [Text]
  | ExtraTrustedUsers [Text]
  | TrustedPublicKeys [Text]
  | ExtraTrustedPublicKeys [Text]
  | NetRcFile Text
  | Include IncludeType
  | -- | A line whose meaning is known but whose raw text must be preserved:
    -- assignments with inline '#' comments and Nix 1.0 alias keys. The parsed
    -- meaning is visible to reads (trust detection, substituter checks), but
    -- the line renders back byte for byte and is never rewritten or migrated.
    Verbatim NixConfLine Text
  | Other Text
  deriving (Show, Eq)

data IncludeType
  = RequiredInclude Text -- for "include"
  | OptionalInclude Text -- for "!include"
  deriving (Show, Eq)

-- | Whether legacy cache settings an older cachix wrote inline into nix.conf
-- may be migrated into the fragment. Migration rewrites nix.conf, so it must
-- be off when the file cannot be written (e.g. managed by home-manager or
-- nix-darwin), or every run would fail trying to strip the same lines.
data MigrateLegacy = MigrateLegacy | LeaveLegacy
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

data NixConfError
  = -- | Error when trying to read the nix.conf file or an include
    IOError FilePath System.IO.Error.IOError
  | -- | Failed to parse the nix.conf file
    ParseError FilePath Text
  deriving (Show, Typeable)

-- | Operations on nix.conf.
-- Helps to work with both NixConf and NixConfSource.
class NixConfOps a where
  -- | Read the nix.conf lines that match the given predicate
  readLines :: (NixConfLine -> Maybe [Text]) -> a -> [Text]

  -- | Write the given lines to the nix.conf
  writeLines :: (NixConfLine -> Maybe [Text]) -> NixConfLine -> a -> a

  -- | Render the nix.conf to a Text
  render :: a -> Text

instance NixConfOps NixConf where
  readLines predicate (NixConf xs) = foldl' f [] xs
    where
      f :: [Text] -> NixConfLine -> [Text]
      f prev next = prev <> fromMaybe [] (predicate next)

  writeLines predicate addition = fmap f
    where
      f x = filter (isNothing . predicate) x <> [addition]

  render (NixConf ls) = T.unlines $ fmap go ls
    where
      go :: NixConfLine -> Text
      go (Substituters xs) = "substituters" <> " = " <> T.unwords xs
      go (ExtraSubstituters xs) = "extra-substituters" <> " = " <> T.unwords xs
      go (TrustedUsers xs) = "trusted-users = " <> T.unwords xs
      go (ExtraTrustedUsers xs) = "extra-trusted-users = " <> T.unwords xs
      go (TrustedPublicKeys xs) = "trusted-public-keys" <> " = " <> T.unwords xs
      go (ExtraTrustedPublicKeys xs) = "extra-trusted-public-keys" <> " = " <> T.unwords xs
      go (NetRcFile filename) = "netrc-file = " <> filename
      go (Include (RequiredInclude path)) = "include " <> path
      go (Include (OptionalInclude path)) = "!include " <> path
      go (Verbatim _ raw) = raw
      go (Other line) = line

instance NixConfOps NixConfSource where
  readLines f = readLines f . nixConfLines
  writeLines f = fmap . writeLines f
  render = render . nixConfLines

isSubstituter :: NixConfLine -> Maybe [Text]
isSubstituter (Substituters xs) = Just xs
isSubstituter (ExtraSubstituters xs) = Just xs
isSubstituter (Verbatim line _) = isSubstituter line
isSubstituter _ = Nothing

isPublicKey :: NixConfLine -> Maybe [Text]
isPublicKey (TrustedPublicKeys xs) = Just xs
isPublicKey (ExtraTrustedPublicKeys xs) = Just xs
isPublicKey (Verbatim line _) = isPublicKey line
isPublicKey _ = Nothing

isTrustedUsers :: NixConfLine -> Maybe [Text]
isTrustedUsers (TrustedUsers xs) = Just xs
isTrustedUsers (ExtraTrustedUsers xs) = Just xs
isTrustedUsers (Verbatim line _) = isTrustedUsers line
isTrustedUsers _ = Nothing

-- | Restrict a predicate to lines cachix may rewrite: lines preserved
-- verbatim (inline comments, alias keys) are never collected into the
-- fragment or replaced by it.
owned :: (NixConfLine -> Maybe [Text]) -> NixConfLine -> Maybe [Text]
owned _ (Verbatim _ _) = Nothing
owned predicate line = predicate line

-- | Replace matching assignments, omitting the replacement when it has no values.
writeNonEmptyLines :: (NixConfLine -> Maybe [Text]) -> NixConfLine -> NixConf -> NixConf
writeNonEmptyLines predicate addition =
  case predicate addition of
    Just [] -> fmap $ filter (isNothing . predicate)
    _ -> writeLines predicate addition

-- | Write the given substituters and public keys as extra-* assignments,
-- leaving any other existing settings untouched.
writeExtraCaches :: [Text] -> [Text] -> NixConf -> NixConf
writeExtraCaches substituters publicKeys =
  writeNonEmptyLines (owned isPublicKey) (ExtraTrustedPublicKeys $ nub publicKeys)
    . writeNonEmptyLines (owned isSubstituter) (ExtraSubstituters $ nub substituters)

-- | The caches cachix manages in the fragment it owns: every substituter and
-- public key written there. The whole fragment belongs to cachix, so every
-- entry counts regardless of value, including a restated Nix default carried
-- over from migrated legacy settings.
fragmentCaches :: NixConf -> ([Text], [Text])
fragmentCaches conf =
  ( readLines (owned isSubstituter) conf,
    readLines (owned isPublicKey) conf
  )

-- | Substituter values in a line an OLDER cachix wrote inline into nix.conf.
-- Old versions always wrote @defaultPublicURI@ as the FIRST value of the
-- plain @substituters =@ line they produced (to work around the line
-- overriding Nix's defaults), so a leading default acts as a per line
-- provenance marker. The default is kept in the migrated values: the old
-- line overrode substituters set at other config levels, so restating the
-- default in the additive extra-* settings is the only way to guarantee it
-- stays reachable afterwards. Lines whose first value is not the default
-- (however common the default is in user-authored lines), and extra-* lines
-- (which old cachix never wrote), are left alone.
legacySubstituters :: NixConfLine -> Maybe [Text]
legacySubstituters (Substituters xs@(marker : _))
  | marker == defaultPublicURI = Just xs
legacySubstituters _ = Nothing

-- | Same as `legacySubstituters` for the trusted-public-keys line, marked by
-- a leading @defaultSigningKey@.
legacyPublicKeys :: NixConfLine -> Maybe [Text]
legacyPublicKeys (TrustedPublicKeys xs@(marker : _))
  | marker == defaultSigningKey = Just xs
legacyPublicKeys _ = Nothing

-- | The caches an older cachix wrote inline into the nix.conf. Old versions
-- always wrote BOTH marker-led lines together, so a lone marked line (a shape
-- users also write by hand, e.g. a substituters override listing the default
-- first) is not claimed.
legacyCaches :: NixConf -> ([Text], [Text])
legacyCaches conf =
  case (readLines legacySubstituters conf, readLines legacyPublicKeys conf) of
    (substituters@(_ : _), publicKeys@(_ : _)) -> (substituters, publicKeys)
    _ -> ([], [])

-- | The name of the config fragment cachix owns. It lives next to the nix.conf
-- it configures and is pulled in with an @!include@, so cachix only ever writes
-- its own file and never rewrites the user's settings.
cachixConf :: Text
cachixConf = "cachix.conf"

-- | Append the cache's substituter and public keys to the collected values
-- and write them back as extra-* assignments.
appendCache :: BinaryCache.BinaryCache -> ([Text], [Text]) -> NixConf -> NixConf
appendCache bc (substituters, publicKeys) =
  writeExtraCaches
    (substituters <> [BinaryCache.uri bc])
    (publicKeys <> BinaryCache.publicSigningKeys bc)

-- | Add a binary cache. Given the nix.conf (at the given path) and the cachix
-- fragment it includes, returns the updated @(nix.conf, fragment)@. The
-- cache's substituter and public keys are written to the fragment as extra-*
-- settings, the nix.conf gains an @!include@ of the fragment, and (when
-- migration is on) cache settings older versions wrote inline in the nix.conf
-- are moved into the fragment.
addCache :: BinaryCache.BinaryCache -> MigrateLegacy -> FilePath -> NixConf -> NixConf -> (NixConf, NixConf)
addCache bc migrate nixConfPath nixconf fragment =
  (nixconf', appendCache bc (collectCaches migrate nixconf fragment) fragment)
  where
    nixconf' = ensureOptionalInclude nixConfPath (stripLegacy nixconf)
    stripLegacy = case migrate of
      MigrateLegacy -> stripCaches
      LeaveLegacy -> identity

-- | Remove a binary cache from the nix.conf and its fragment. Returns the
-- updated configs and whether the cache was present. When it is absent both
-- configs are returned untouched.
removeCache :: URI.URI -> Text -> MigrateLegacy -> FilePath -> NixConf -> NixConf -> ((NixConf, NixConf), Bool)
removeCache uri name migrate nixConfPath nixconf fragment
  | removed = ((nixconf', fragment'), True)
  | otherwise = ((nixconf, fragment), False)
  where
    (fragment', removed) = removeFrom uri name (collectCaches migrate nixconf fragment) fragment
    migratesLegacy = migrate == MigrateLegacy && legacyCaches nixconf /= ([], [])
    -- A removal only bookkeeps the nix.conf: legacy lines it migrates are
    -- stripped, and the include is ensured only when that migration moves
    -- active inline settings into the fragment (and the fragment has content;
    -- an emptied fragment must not gain an include pointing at a file never
    -- written). A plain removal never adds the include, so it cannot
    -- re-activate remaining fragment caches on a system where the include
    -- was deliberately absent.
    nixconf'
      | not migratesLegacy = nixconf
      | fragment' == NixConf [] = stripCaches nixconf
      | otherwise = ensureOptionalInclude nixConfPath (stripCaches nixconf)

-- | Drop the given cache from the collected values and write the remainder
-- back as extra-* assignments, reporting whether anything matched.
removeFrom :: URI.URI -> Text -> ([Text], [Text]) -> NixConf -> (NixConf, Bool)
removeFrom uri name collected conf
  | removed = (writeExtraCaches substituters publicKeys conf, True)
  | otherwise = (conf, False)
  where
    ((substituters, publicKeys), removed) = filterCache uri name collected

-- | Drop the given cache's substituter and public keys from the collected
-- values, reporting whether either matched. Public keys count on their own:
-- a leftover key must stay removable even when its substituter is already
-- gone, since a stale key keeps the cache's signatures trusted.
filterCache :: URI.URI -> Text -> ([Text], [Text]) -> (([Text], [Text]), Bool)
filterCache uri name (oldSubstituters, oldPublicKeys) =
  ((substituters, publicKeys), removed)
  where
    substituters = filter (URI.serialize fulluri /=) oldSubstituters
    publicKeys = filter (not . T.isPrefixOf (toS $ URI.hostBS $ URI.getHostname fulluri)) oldPublicKeys
    removed = substituters /= oldSubstituters || publicKeys /= oldPublicKeys
    fulluri = URI.appendSubdomain name uri

-- | Add a binary cache to a self-contained nix.conf, e.g. one generated with
-- @--output-directory@. The whole file is cachix-generated, so its cache
-- lines (including plain assignments written by older versions) are collected
-- and rewritten as extra-* settings in place; no fragment or include is
-- involved, keeping the file usable on its own.
addCacheStandalone :: BinaryCache.BinaryCache -> NixConf -> NixConf
addCacheStandalone bc conf = appendCache bc (fragmentCaches conf) conf

-- | Remove a binary cache from a self-contained nix.conf.
-- See `addCacheStandalone`.
removeCacheStandalone :: URI.URI -> Text -> NixConf -> (NixConf, Bool)
removeCacheStandalone uri name conf = removeFrom uri name (fragmentCaches conf) conf

-- | The caches cachix manages across the nix.conf and its fragment: legacy
-- inline settings recognized in the nix.conf (when migration is on), plus
-- everything in the fragment.
collectCaches :: MigrateLegacy -> NixConf -> NixConf -> ([Text], [Text])
collectCaches migrate nixconf fragment =
  let (s1, k1) = case migrate of
        MigrateLegacy -> legacyCaches nixconf
        LeaveLegacy -> ([], [])
      (s2, k2) = fragmentCaches fragment
   in (s1 <> s2, k1 <> k2)

-- | Remove exactly the lines `legacyCaches` collects: assignments an older
-- cachix wrote inline. When nothing is claimed (including when only one of
-- the two marker lines is present), the nix.conf is left untouched.
stripCaches :: NixConf -> NixConf
stripCaches conf
  | legacyCaches conf == ([], []) = conf
  | otherwise = fmap (filter (\l -> isNothing (legacySubstituters l) && isNothing (legacyPublicKeys l))) conf

-- | Ensure the nix.conf at the given path optionally includes the cachix
-- fragment, appending the directive when no existing include already resolves
-- to it. Includes are resolved relative to the nix.conf's directory, the same
-- way `resolveIncludesWithStack` does, so relative ("cachix.conf",
-- "./cachix.conf") and absolute spellings all count.
ensureOptionalInclude :: FilePath -> NixConf -> NixConf
ensureOptionalInclude nixConfPath conf@(NixConf ls)
  | any isIncludeOf ls = conf
  | otherwise = NixConf (ls <> [Include (OptionalInclude cachixConf)])
  where
    dir = takeDirectory nixConfPath
    fragmentPath = normalise (dir </> toS cachixConf)
    isIncludeOf (Include (RequiredInclude q)) = matches q
    isIncludeOf (Include (OptionalInclude q)) = matches q
    isIncludeOf (Verbatim line _) = isIncludeOf line
    isIncludeOf _ = False
    matches q = normalise (dir </> toS q) == fragmentPath

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
  includedConfigs <- mapM resolveInclude (mapMaybe includeOf ls)
  return $ baseConf : concat includedConfigs
  where
    dir = takeDirectory baseFile

    -- Includes carrying an inline comment still pull in their file.
    includeOf (Include f) = Just f
    includeOf (Verbatim line _) = includeOf line
    includeOf _ = Nothing

    resolveInclude :: IncludeType -> IO [NixConfSource]
    resolveInclude includeType = do
      let path = case includeType of
            RequiredInclude p -> p
            OptionalInclude p -> p
          fullPath = normalise $ dir </> toS path

      if fullPath `elem` stack
        then case includeType of
          RequiredInclude _ ->
            throwIO $ CircularInclude (formatCircularError fullPath)
          OptionalInclude _ -> return []
        else do
          read' fullPath >>= \case
            Left err@(IOError _ _) ->
              if isRequired includeType
                then do
                  printNixConfError err
                  throwIO $ IncludeNotFound ("Failed to read required include file: " <> toS fullPath)
                else return []
            Left err@(ParseError _ _) -> do
              printNixConfError err
              throwIO $ IncludeNotFound ("Failed to read required include file: " <> toS fullPath)
            Right conf ->
              resolveIncludesWithStack (fullPath : stack) baseFile conf

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

-- | Safely read a nix.conf file from the given location.
-- Prints errors to stderr.
read :: NixConfLoc -> IO (Maybe NixConfSource)
read ncl = do
  filename <- getFilename ncl
  read' filename >>= \case
    Left err -> do
      printNixConfError err
      return Nothing
    Right conf -> return $ Just conf

-- | Safely read a nix.conf file from the given location.
-- Return an empty NixConfSource if the file does not exist or cannot be read.
-- Prints errors to stderr.
readWithDefault :: NixConfLoc -> IO NixConfSource
readWithDefault ncl = readPathWithDefault =<< getFilename ncl

-- | Read a nix.conf, defaulting to an empty one (with the given path) when
-- the file does not exist; noisy controls whether that case prints a note.
-- Any other failure reading an EXISTING file is fatal: falling back to an
-- empty config would let the next write silently replace the file's contents.
readPath :: Bool -> FilePath -> IO NixConfSource
readPath noisy filename =
  read' filename >>= \case
    Left err@(IOError _ ioerr)
      | isDoesNotExistError ioerr -> do
          when noisy $ printNixConfError err
          return $ new filename
    Left err -> throwNixConfError err
    Right conf -> return conf

-- | See `readPath`.
readPathWithDefault :: FilePath -> IO NixConfSource
readPathWithDefault = readPath True

-- | Like `readPathWithDefault`, but silent when the file simply doesn't
-- exist. Meant for files cachix exclusively manages, such as the cachix.conf
-- fragment, which is expected to be missing until the first `cachix use`,
-- so printing a "no config" error for it would just be noise.
readPathQuiet :: FilePath -> IO NixConfSource
readPathQuiet = readPath False

-- | Rethrow a read failure as a CachixException, so the top-level handler
-- prints it exactly once.
throwNixConfError :: NixConfError -> IO a
throwNixConfError err@(IOError _ _) = throwIO $ NixConfReadFailed (formatNixConfError err)
throwNixConfError err@(ParseError _ _) = throwIO $ NixConfParseError (formatNixConfError err)

-- | Safely read a nix.conf file from the given file path.
read' :: FilePath -> IO (Either NixConfError NixConfSource)
read' filename = do
  econtent <- Safe.tryIO (readFile filename)
  return $ case econtent of
    Left err -> Left $ IOError filename err
    Right content ->
      case parse content of
        Left err -> Left $ ParseError filename $ toS (Mega.errorBundlePretty err)
        Right conf -> Right $ NixConfSource filename conf

getFilename :: NixConfLoc -> IO FilePath
getFilename ncl = do
  dir <-
    case ncl of
      Global -> return "/etc/nix"
      Local -> getXdgDirectory XdgConfig "nix"
      Custom filepath -> return filepath
  return $ dir <> "/nix.conf"

formatNixConfError :: NixConfError -> Text
formatNixConfError (IOError path err)
  | isDoesNotExistError err =
      unlines
        [ "No config at " <> toS path <> ":",
          "",
          toS (displayException err)
        ]
  | otherwise =
      unlines
        [ "Failed to read " <> toS path <> ":",
          "",
          toS (displayException err)
        ]
formatNixConfError (ParseError path err) =
  unlines
    [ "Failed to parse " <> toS path <> ":",
      "",
      err
    ]

printNixConfError :: NixConfError -> IO ()
printNixConfError = putErrText . formatNixConfError

-- nix.conf Parser
type Parser = Mega.Parsec Void Text

-- | Parse an assignment cachix may manage. A line carrying an inline '#'
-- comment is wrapped in Verbatim: its values (up to the '#', as Nix reads
-- them) stay visible, but the line renders back unchanged.
parseLine :: ([Text] -> NixConfLine) -> Text -> Parser NixConfLine
parseLine = parseAssignment False

-- | Parse an assignment cachix only ever reads (Nix 1.0 alias keys): the
-- parsed meaning is always wrapped in Verbatim, so a rewrite reproduces the
-- user's line instead of renaming it to the canonical key.
parseReadOnlyLine :: ([Text] -> NixConfLine) -> Text -> Parser NixConfLine
parseReadOnlyLine = parseAssignment True

parseAssignment :: Bool -> ([Text] -> NixConfLine) -> Text -> Parser NixConfLine
parseAssignment alwaysVerbatim constr name = Mega.try $ do
  (raw, values) <- Mega.match $ do
    _ <- optional (some (char ' '))
    _ <- string name
    _ <- many (char ' ')
    _ <- char '='
    _ <- many (char ' ')
    Mega.sepBy1 (many (Mega.satisfy (not . isSpace))) (some (char ' '))
  _ <- many (char ' ')
  _ <- void eol <|> Mega.eof
  let vals = fmap toS values :: [Text]
      hasComment = any (T.isInfixOf "#") vals
      semantic = constr (stripInlineComment vals)
  return $
    if alwaysVerbatim || hasComment
      then Verbatim semantic raw
      else semantic

-- | Values as Nix sees them: everything from the first '#' to the end of the
-- line is a comment, and empty tokens (from stray spacing) are dropped.
stripInlineComment :: [Text] -> [Text]
stripInlineComment = filter (not . T.null) . go
  where
    go [] = []
    go (v : vs)
      | "#" `T.isInfixOf` v = [T.takeWhile (/= '#') v]
      | otherwise = v : go vs

parseInclude :: (Text -> IncludeType) -> Text -> Parser NixConfLine
parseInclude constr name = Mega.try $ do
  (raw, path) <- Mega.match $ do
    _ <- optional (some (char ' '))
    _ <- string name
    _ <- some (char ' ')
    many (Mega.satisfy (not . isSpace))
  trailing <- many (Mega.satisfy (/= '\n'))
  _ <- void eol <|> Mega.eof
  let pathT = toS path :: Text
      include = Include (constr (T.takeWhile (/= '#') pathT))
      rest = T.strip (toS trailing)
      rawLine = raw <> toS trailing
  if T.null rest || "#" `T.isPrefixOf` rest
    then
      return $
        if "#" `T.isInfixOf` pathT || not (T.null rest)
          then Verbatim include rawLine
          else include
    else empty

parseOther :: Parser NixConfLine
parseOther = Mega.try $ Other . toS <$> Mega.someTill Mega.anySingle (void eol <|> Mega.eof)

parseAltLine :: Parser NixConfLine
parseAltLine =
  (Other "" <$ eol)
    <|> parseLine ExtraSubstituters "extra-substituters"
    <|> parseLine Substituters "substituters"
    <|> parseLine ExtraTrustedPublicKeys "extra-trusted-public-keys"
    <|> parseLine TrustedPublicKeys "trusted-public-keys"
    <|> parseLine ExtraTrustedUsers "extra-trusted-users"
    <|> parseLine TrustedUsers "trusted-users"
    -- Nix 1.0 alias keys are still honored by Nix, so they are read (the
    -- substituter checks must see them), but never rewritten: rendering them
    -- back as canonical keys would rename user-authored lines.
    <|> parseReadOnlyLine Substituters "binary-caches"
    <|> parseReadOnlyLine TrustedPublicKeys "binary-cache-public-keys"
    -- NB: assume that space in this option means space in filename
    <|> parseLine (NetRcFile . T.unwords) "netrc-file"
    <|> parseInclude RequiredInclude "include"
    <|> parseInclude OptionalInclude "!include"
    <|> parseOther

parser :: Parser NixConf
parser = NixConf <$> many parseAltLine

parse :: Text -> Either (Mega.ParseErrorBundle Text Void) NixConf
parse = Mega.parse parser "nix.conf"
