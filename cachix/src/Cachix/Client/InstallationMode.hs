{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.InstallationMode
  ( InstallationMode (..),
    NixEnv (..),
    getNixEnv,
    requireNixEnv,
    getInstallationMode,
    addBinaryCache,
    removeBinaryCache,
    isTrustedUser,
    getUser,
    fromString,
    toString,
    UseOptions (..),
    defaultUseOptions,
  )
where

import Cachix.API.Error (escalateAs)
import Cachix.Client.Config (Config)
import Cachix.Client.Config qualified as Config
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.NetRc qualified as NetRc
import Cachix.Client.NixConf qualified as NixConf
import Cachix.Client.NixVersion (assertNixVersion)
import Cachix.Client.URI qualified as URI
import Cachix.Types.BinaryCache qualified as BinaryCache
import Data.Maybe qualified
import Data.String.Here
import Data.Text qualified as T
import Protolude hiding (toS)
import Protolude.Conv (toS)
import System.Directory (Permissions, createDirectoryIfMissing, doesFileExist, getPermissions, writable)
import System.Environment (lookupEnv)
import System.FilePath (replaceFileName, (</>))
import System.Process (readProcessWithExitCode)
import Prelude (String)

data NixEnv = NixEnv
  { isTrusted :: Bool,
    isRoot :: Bool,
    isNixOS :: Bool
  }

-- NOTE: update the list of options for --mode argument in OptionsParser.hs
data InstallationMode
  = Install NixConf.NixConfLoc
  | WriteNixOS
  | UntrustedRequiresSudo
  | UntrustedNixOS
  deriving (Show, Eq)

data UseOptions = UseOptions
  { useMode :: Maybe InstallationMode,
    useNixOSFolder :: FilePath,
    useOutputDirectory :: Maybe FilePath
  }
  deriving (Show)

defaultUseOptions :: UseOptions
defaultUseOptions =
  UseOptions
    { useMode = Nothing,
      useNixOSFolder = "/etc/nixos",
      useOutputDirectory = Nothing
    }

fromString :: String -> Maybe InstallationMode
fromString "root-nixconf" = Just $ Install NixConf.Global
fromString "user-nixconf" = Just $ Install NixConf.Local
fromString "nixos" = Just WriteNixOS
fromString "untrusted-requires-sudo" = Just UntrustedRequiresSudo
fromString _ = Nothing

toString :: InstallationMode -> String
toString (Install NixConf.Global) = "root-nixconf"
toString (Install NixConf.Local) = "user-nixconf"
toString (Install (NixConf.Custom _)) = "custom-nixconf"
toString WriteNixOS = "nixos"
toString UntrustedRequiresSudo = "untrusted-requires-sudo"
toString UntrustedNixOS = "untrusted-nixos"

getNixEnv :: IO NixEnv
getNixEnv = do
  user <- getUser
  -- The global nix.conf is only inspected for trusted users here, never
  -- written back, so an unreadable file can safely degrade to an empty one.
  globalConf <-
    NixConf.read NixConf.Global
      >>= maybe (NixConf.new <$> NixConf.getFilename NixConf.Global) pure
  ncs <- NixConf.resolveIncludes globalConf
  isTrusted <- isTrustedUser $ concatMap (NixConf.readLines NixConf.isTrustedUsers) ncs
  isNixOS <- doesFileExist "/run/current-system/nixos-version"
  return $
    NixEnv
      { isRoot = user == "root",
        isTrusted = isTrusted,
        isNixOS = isNixOS
      }

-- | Assert the installed Nix is new enough, then resolve the environment
-- both `cachix use` and `cachix remove` need before touching nix.conf.
requireNixEnv :: IO NixEnv
requireNixEnv = do
  () <- escalateAs UnsupportedNixVersion =<< assertNixVersion
  getNixEnv

getInstallationMode :: NixEnv -> UseOptions -> InstallationMode
getInstallationMode nixenv useOptions
  | (isRoot nixenv || isTrusted nixenv) && isJust (useOutputDirectory useOptions) = Install (NixConf.Custom $ Data.Maybe.fromJust $ useOutputDirectory useOptions)
  | isJust (useMode useOptions) = Data.Maybe.fromJust $ useMode useOptions
  | isNixOS nixenv && isRoot nixenv = WriteNixOS
  | not (isNixOS nixenv) && isRoot nixenv = Install NixConf.Global
  | isTrusted nixenv = Install NixConf.Local
  | isNixOS nixenv = UntrustedNixOS
  | otherwise = UntrustedRequiresSudo

-- | Add a Binary cache to nix.conf, print nixos config or fail
addBinaryCache :: Config -> BinaryCache.BinaryCache -> UseOptions -> InstallationMode -> IO ()
addBinaryCache _ _ _ UntrustedNixOS = do
  user <- getUser
  throwIO $
    MustBeRoot
      [i|This user doesn't have permissions to configure binary caches.

You can either:

a) Run the same command as root to write NixOS configuration.

b) Add the following to your configuration.nix to add your user as trusted 
   and then try again:

  nix.settings.trusted-users = [ "root" "${user}" ];

|]
addBinaryCache _ _ _ UntrustedRequiresSudo = do
  user <- getUser
  throwIO $
    MustBeRoot
      [i|This user doesn't have permissions to configure binary caches.

You can either:

a) Run the same command as root to configure them globally.

b) Run the following command to add your user as trusted 
   and then try again:

  echo "trusted-users = root ${user}" | sudo tee -a /etc/nix/nix.conf && sudo pkill nix-daemon
|]
addBinaryCache config bc useOptions WriteNixOS =
  nixosBinaryCache config bc useOptions
-- A custom output directory (--output-directory) is a fully cachix-generated
-- artifact meant to be shipped elsewhere, so its nix.conf stays
-- self-contained instead of splitting the caches into a fragment.
addBinaryCache config bc _ (Install ncl@(NixConf.Custom _)) = do
  nixConf <- NixConf.readWithDefault ncl
  unless (BinaryCache.isPublic bc) $ void $ addPrivateBinaryCacheNetRC config bc ncl
  _ <- writeWithHint outputDirHint nixConf (nixConf {NixConf.nixConfLines = NixConf.addCacheStandalone bc (NixConf.nixConfLines nixConf)})
  putStrLn $ "Configured " <> BinaryCache.uri bc <> " binary cache in " <> toS (NixConf.nixConfPath nixConf)
addBinaryCache config bc _ (Install ncl) = do
  (nixConf, fragment) <- resolveNixConfAndFragment ncl
  migrate <- migrateLegacyMode nixConf
  netrcLocMaybe <- forM (guard $ not (BinaryCache.isPublic bc)) $ const $ addPrivateBinaryCacheNetRC config bc ncl
  let managedNetRC = toS (replaceFileName (NixConf.nixConfPath nixConf) "netrc") :: Text
      NixConf.NixConf nixConfLs = NixConf.nixConfLines nixConf
      -- Older cachix wrote the netrc-file line straight into nix.conf.
      hasLegacyNetRC = NixConf.NetRcFile managedNetRC `elem` nixConfLs
      -- We only manage the netrc line for local user configs for now.
      -- On NixOS we assume it will be picked up from the default location.
      addNetRCLine :: NixConf.NixConfSource -> NixConf.NixConfSource
      addNetRCLine
        | ncl /= NixConf.Local = identity
        | Just netrcLoc <- netrcLocMaybe = setNetRC (toS netrcLoc)
        -- Keep a legacy netrc line working by moving it into the fragment,
        -- but only when nix.conf is writable so the stale copy can also be
        -- dropped; otherwise the line simply keeps working where it is.
        | hasLegacyNetRC, NixConf.MigrateLegacy <- migrate = setNetRC managedNetRC
        | otherwise = identity
      -- Drop the stale copy from nix.conf once the fragment carries it. Only
      -- the exact path cachix manages is matched, so a netrc-file setting the
      -- user authored themselves is left alone.
      clearStaleNetRCLine :: NixConf.NixConfSource -> NixConf.NixConfSource
      clearStaleNetRCLine
        | ncl == NixConf.Local, NixConf.MigrateLegacy <- migrate = clearNetRC managedNetRC
        | otherwise = identity
      (nixConf', fragment') = NixConf.addCache bc migrate (NixConf.nixConfPath nixConf) (NixConf.nixConfLines nixConf) (NixConf.nixConfLines fragment)
      nixConfPathT = toS (NixConf.nixConfPath nixConf) :: Text
      fragmentPathT = toS (NixConf.nixConfPath fragment) :: Text
      includeHint =
        [iTrim|
The caches are configured in ${fragmentPathT}, but ${nixConfPathT} could not be updated to include that file.
If your nix.conf is managed by home-manager or nix-darwin, add the following line to it and re-run this command:

  !include ${NixConf.cachixConf}
|]
  printMigrationNotice migrate nixConf fragment
  _ <- writeWithHint fragmentWriteHint fragment (addNetRCLine (fragment {NixConf.nixConfLines = fragment'}))
  _ <- writeWithHint includeHint nixConf (clearStaleNetRCLine (nixConf {NixConf.nixConfLines = nixConf'}))
  putStrLn $ "Configured " <> BinaryCache.uri bc <> " binary cache in " <> fragmentPathT

removeBinaryCache :: URI.URI -> Text -> InstallationMode -> IO ()
removeBinaryCache uri name (Install ncl@(NixConf.Custom _)) = do
  nixConf <- NixConf.readWithDefault ncl
  let (nixConf', removed) = NixConf.removeCacheStandalone uri name (NixConf.nixConfLines nixConf)
  if removed
    then do
      _ <- writeWithHint outputDirHint nixConf (nixConf {NixConf.nixConfLines = nixConf'})
      putStrLn $ "Removed " <> host uri name <> " binary cache from " <> (toS (NixConf.nixConfPath nixConf) :: Text)
    else putStrLn $ "No " <> host uri name <> " binary cache found in " <> (toS (NixConf.nixConfPath nixConf) :: Text)
removeBinaryCache uri name (Install ncl) = do
  (nixConf, fragment) <- resolveNixConfAndFragment ncl
  migrate <- migrateLegacyMode nixConf
  let cacheUri = host uri name
      ((nixConf', fragment'), removed) =
        NixConf.removeCache uri name migrate (NixConf.nixConfPath nixConf) (NixConf.nixConfLines nixConf) (NixConf.nixConfLines fragment)
      nixConfPathT = toS (NixConf.nixConfPath nixConf) :: Text
      fragmentPathT = toS (NixConf.nixConfPath fragment) :: Text
  if removed
    then do
      printMigrationNotice migrate nixConf fragment
      fragmentWritten <- writeWithHint fragmentWriteHint fragment (fragment {NixConf.nixConfLines = fragment'})
      nixConfWritten <- writeWithHint (removeNixConfHint fragmentPathT) nixConf (nixConf {NixConf.nixConfLines = nixConf'})
      let changed =
            [fragmentPathT | fragmentWritten]
              <> [nixConfPathT | nixConfWritten]
      putStrLn $
        "Removed "
          <> cacheUri
          <> " binary cache"
          <> (if null changed then "" else " from " <> T.intercalate " and " changed)
      -- The cache may also sit in a line cachix does not manage; removing
      -- the managed entry alone would leave it silently active.
      when (stillConfigured cacheUri nixConf') $
        putStrLn $
          "Note: " <> cacheUri <> " is still configured in " <> nixConfPathT <> " by a line cachix does not manage. Remove it manually."
    else
      if stillConfigured cacheUri (NixConf.nixConfLines nixConf)
        then
          putStrLn $
            "Found "
              <> cacheUri
              <> " in "
              <> nixConfPathT
              <> ", but cachix does not manage that entry. Remove it manually."
        else
          putStrLn $
            "No "
              <> cacheUri
              <> " binary cache found in "
              <> nixConfPathT
              <> " or "
              <> fragmentPathT
removeBinaryCache _ _ _ = do
  throwIO $ RemoveCacheUnsupported "Removing binary caches is only supported for nix.conf"

-- | The full substituter URI of the given cache, as it appears in nix.conf.
host :: URI.URI -> Text -> Text
host uri name = URI.serialize (URI.appendSubdomain name uri)

-- | Whether the substituter appears (modulo a trailing slash) among the
-- substituter lines of the given config, including lines cachix does not
-- manage such as ones with inline comments or Nix 1.0 alias keys.
stillConfigured :: Text -> NixConf.NixConf -> Bool
stillConfigured cacheUri conf =
  stripSlash cacheUri `elem` fmap stripSlash (NixConf.readLines NixConf.isSubstituter conf)
  where
    stripSlash = T.dropWhileEnd (== '/')

-- | Resolve the nix.conf at the given location and the cachix.conf fragment
-- it includes, reading both from disk.
resolveNixConfAndFragment :: NixConf.NixConfLoc -> IO (NixConf.NixConfSource, NixConf.NixConfSource)
resolveNixConfAndFragment ncl = do
  nixConfPath <- NixConf.getFilename ncl
  let fragmentPath = replaceFileName nixConfPath (toS NixConf.cachixConf)
  nixConf <- NixConf.readPathWithDefault nixConfPath
  -- The fragment is expected to not exist yet on a first run; read it
  -- quietly rather than printing a "no config" error for that.
  fragment <- NixConf.readPathQuiet fragmentPath
  return (nixConf, fragment)

-- | Write the new NixConfSource only if its lines differ from the old one.
-- Returns whether a write happened.
writeIfChanged :: NixConf.NixConfSource -> NixConf.NixConfSource -> IO Bool
writeIfChanged old new
  | NixConf.nixConfLines old /= NixConf.nixConfLines new = NixConf.write new $> True
  | otherwise = pure False

-- | Whether legacy inline settings may be migrated out of the nix.conf:
-- only when the file can actually be rewritten. A read-only nix.conf (e.g.
-- a home-manager or nix-darwin managed symlink) keeps its legacy lines, so
-- the command does not fail on every run trying to strip them.
migrateLegacyMode :: NixConf.NixConfSource -> IO NixConf.MigrateLegacy
migrateLegacyMode nixConf = do
  isWritable <- isWritablePath (NixConf.nixConfPath nixConf)
  pure $ if isWritable then NixConf.MigrateLegacy else NixConf.LeaveLegacy

isWritablePath :: FilePath -> IO Bool
isWritablePath path = do
  exists <- doesFileExist path
  if exists
    then writable <$> getPermissions path
    else pure True

-- | Tell the user what happens to cache settings an older cachix wrote
-- inline into nix.conf: migrated into the cachix.conf fragment when nix.conf
-- is writable, left in place when it is not.
printMigrationNotice :: NixConf.MigrateLegacy -> NixConf.NixConfSource -> NixConf.NixConfSource -> IO ()
printMigrationNotice migrate nixConf fragment =
  case NixConf.legacyCaches (NixConf.nixConfLines nixConf) of
    ([], []) -> pure ()
    (substituters, publicKeys) ->
      case migrate of
        NixConf.MigrateLegacy -> do
          putErrText $
            "Migrating cache settings written by an older cachix from "
              <> toS (NixConf.nixConfPath nixConf)
              <> " to "
              <> toS (NixConf.nixConfPath fragment)
              <> ":"
          putErrText $ "  substituters: " <> T.unwords substituters
          putErrText $ "  trusted-public-keys: " <> T.unwords publicKeys
        NixConf.LeaveLegacy ->
          putErrText $
            toS (NixConf.nixConfPath nixConf)
              <> " is not writable; leaving cache settings written by an older cachix in place."

-- | Write the conf if it changed, and when the write fails (commonly a
-- read-only or root-owned file) rethrow as a CachixException carrying the
-- given hint instead of dying with a bare IO error.
writeWithHint :: Text -> NixConf.NixConfSource -> NixConf.NixConfSource -> IO Bool
writeWithHint hint old new = do
  result <- try (writeIfChanged old new) :: IO (Either IOException Bool)
  case result of
    Right written -> pure written
    Left err ->
      throwIO $
        NixConfWriteFailed $
          T.intercalate
            "\n"
            [ "Could not write to " <> toS (NixConf.nixConfPath old) <> ":",
              "",
              "  " <> toS (displayException err),
              "",
              hint
            ]

fragmentWriteHint :: Text
fragmentWriteHint = "Check that you have write access to the file. To configure the system-wide nix.conf, re-run the command with sudo."

outputDirHint :: Text
outputDirHint = "Check that you have write access to the --output-directory path."

removeNixConfHint :: Text -> Text
removeNixConfHint fragmentPath =
  "The cache settings in "
    <> fragmentPath
    <> " were updated, but nix.conf could not be. If your nix.conf is managed by home-manager or nix-darwin, apply the remaining change there manually."

setNetRC :: Text -> NixConf.NixConfSource -> NixConf.NixConfSource
setNetRC netrc = (fmap . fmap) (\ls -> filter noNetRc ls ++ [NixConf.NetRcFile netrc])
  where
    noNetRc (NixConf.NetRcFile _) = False
    noNetRc _ = True

-- | Drop a stale netrc-file line an older cachix wrote into nix.conf,
-- matching the exact path cachix manages so a netrc-file setting the user
-- authored themselves is left alone.
clearNetRC :: Text -> NixConf.NixConfSource -> NixConf.NixConfSource
clearNetRC netrc = (fmap . fmap) (filter keep)
  where
    keep (NixConf.NetRcFile path) = path /= netrc
    keep _ = True

nixosBinaryCache :: Config -> BinaryCache.BinaryCache -> UseOptions -> IO ()
nixosBinaryCache config bc UseOptions {useNixOSFolder = baseDirectory} = do
  _ <- try $ createDirectoryIfMissing True $ toS toplevel :: IO (Either SomeException ())
  eitherPermissions <- try $ getPermissions (toS toplevel) :: IO (Either SomeException Permissions)
  case eitherPermissions of
    Left _ -> throwIO $ NixOSInstructions $ noEtcPermissionInstructions $ toS baseDirectory
    Right permissions
      | writable permissions -> installFiles
      | otherwise -> throwIO $ NixOSInstructions $ noEtcPermissionInstructions $ toS baseDirectory
  where
    installFiles = do
      writeFile (toS glueModuleFile) glueModule
      writeFile (toS cacheModuleFile) cacheModule
      unless (BinaryCache.isPublic bc) $ void $ addPrivateBinaryCacheNetRC config bc NixConf.Global
      putText instructions
    configurationNix :: Text
    configurationNix = toS $ toS baseDirectory </> "configuration.nix"
    namespace :: Text
    namespace = "cachix"
    toplevel :: Text
    toplevel = toS $ toS baseDirectory </> toS namespace
    glueModuleFile :: Text
    glueModuleFile = toplevel <> ".nix"
    cacheModuleFile :: Text
    cacheModuleFile = toplevel <> "/" <> toS (BinaryCache.name bc) <> ".nix"
    noEtcPermissionInstructions :: Text -> Text
    noEtcPermissionInstructions dir =
      [iTrim|
Could not install NixOS configuration to ${dir} due to lack of write permissions.

Pass `--nixos-folder /etc/mynixos/` as an alternative location with write permissions.
|]
    instructions :: Text
    instructions =
      [iTrim|
Cachix configuration written to ${glueModuleFile}.
Binary cache ${BinaryCache.name bc} configuration written to ${cacheModuleFile}.

To start using cachix add the following to your ${configurationNix}:

    imports = [ ./cachix.nix ];

Then run:

    $ sudo nixos-rebuild switch
|]
    glueModule :: Text
    glueModule =
      [i|
# WARN: this file will get overwritten by $ cachix use <name>
{ pkgs, lib, ... }:

let
  folder = ./cachix;
  toImport = name: value: folder + ("/" + name);
  filterCaches = key: value: value == "regular" && lib.hasSuffix ".nix" key;
  imports = lib.mapAttrsToList toImport (lib.filterAttrs filterCaches (builtins.readDir folder));
in {
  inherit imports;
  nix.settings.substituters = ["https://cache.nixos.org/"];
}
|]
    cacheModule :: Text
    cacheModule =
      [i|
{
  nix = {
    settings = {
      substituters = [
        "${BinaryCache.uri bc}"
      ];
      trusted-public-keys = [
        ${T.intercalate " " (map (\s -> "\"" <> s <> "\"") (BinaryCache.publicSigningKeys bc))}
      ];
    };
  };
}
|]

-- TODO: allow overriding netrc location
addPrivateBinaryCacheNetRC :: Config -> BinaryCache.BinaryCache -> NixConf.NixConfLoc -> IO FilePath
addPrivateBinaryCacheNetRC config bc nixconf = do
  filename <- (`replaceFileName` "netrc") <$> NixConf.getFilename nixconf
  authToken <- Config.getAuthTokenRequired config
  let netrcfile = fromMaybe filename Nothing -- TODO: get netrc from nixconf
  written <- NetRc.add authToken [bc] netrcfile
  when written $
    putErrText $
      "Configured private read access credentials in " <> toS filename
  pure filename

isTrustedUser :: [Text] -> IO Bool
isTrustedUser users = do
  user <- getUser
  -- to detect single user installations
  permissions <- getPermissions "/nix/store"
  isInAGroup <- userInAnyGroup user
  return $ writable permissions || user `elem` users || isInAGroup
  where
    groups :: [Text]
    groups = map T.tail $ filter (\u -> (fst <$> T.uncons u) == Just '@') users
    userInAnyGroup :: Text -> IO Bool
    userInAnyGroup user = do
      isIn <- for groups $ checkUserInGroup user
      return $ any identity isIn
    checkUserInGroup :: Text -> Text -> IO Bool
    checkUserInGroup user groupName = do
      (_exitcode, out, _err) <- readProcessWithExitCode "id" ["-Gn", toS user] mempty
      return $ groupName `T.isInfixOf` toS out

getUser :: IO Text
getUser = do
  maybeUser <- lookupEnv "USER"
  case maybeUser of
    Nothing -> throwIO $ UserEnvNotSet "$USER must be set. If running in a container, try setting USER=root."
    Just user -> return $ toS user
