{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.InstallationMode
  ( InstallationMode (..),
    NixEnv (..),
    getNixEnv,
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

import Cachix.Client.Config (Config)
import Cachix.Client.Config qualified as Config
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.NetRc qualified as NetRc
import Cachix.Client.NixConf qualified as NixConf
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
  ncs <- NixConf.resolveIncludes =<< NixConf.readWithDefault NixConf.Global
  isTrusted <- isTrustedUser $ NixConf.readLines ncs NixConf.isTrustedUsers
  isNixOS <- doesFileExist "/run/current-system/nixos-version"
  return $
    NixEnv
      { isRoot = user == "root",
        isTrusted = isTrusted,
        isNixOS = isNixOS
      }

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
addBinaryCache config bc _ (Install ncl) = do
  (input, output) <- prepareNixConf ncl
  netrcLocMaybe <- forM (guard $ not (BinaryCache.isPublic bc)) $ const $ addPrivateBinaryCacheNetRC config bc ncl
  let addNetRCLine :: NixConf.NixConf -> NixConf.NixConf
      addNetRCLine = fromMaybe identity $ do
        netrcLoc <- netrcLocMaybe :: Maybe FilePath
        -- We only add the netrc line for local user configs for now.
        -- On NixOS we assume it will be picked up from the default location.
        guard (ncl == NixConf.Local)
        pure (NixConf.setNetRC $ toS netrcLoc)
  NixConf.write ncl $ addNetRCLine $ NixConf.add bc input output
  filename <- NixConf.getFilename ncl
  putStrLn $ "Configured " <> BinaryCache.uri bc <> " binary cache in " <> toS filename

-- | Resolve and read the nix.conf.
--
-- Returns a set of "input" confs and the "output" conf.
--
-- The output is the parsed conf file at the location specified by the NixConfLoc.
--
-- Inputs are any confs included by the output conf, plus any additional external resolutions.
-- For example, for the local Nix conf, we also return the global one.
prepareNixConf :: NixConf.NixConfLoc -> IO ([NixConf.NixConf], NixConf.NixConf)
prepareNixConf ncl = do
  outputPath <- NixConf.getFilename ncl
  -- TODO: might need locking one day
  gnc <- NixConf.read NixConf.Global
  gncInputs <- traverse NixConf.resolveIncludes gnc

  (input, output) <-
    case ncl of
      NixConf.Global -> do
        return (gncInputs, gnc)
      NixConf.Local -> do
        lnc <- NixConf.read NixConf.Local
        lncInputs <- traverse NixConf.resolveIncludes lnc
        return (gncInputs <> lncInputs, lnc)
      NixConf.Custom _ -> do
        lnc <- NixConf.read ncl
        lncInputs <- traverse NixConf.resolveIncludes lnc
        return (lncInputs, lnc)

  return
    ( fromMaybe [] input,
      fromMaybe (NixConf.new outputPath) output
    )

removeBinaryCache :: URI.URI -> Text -> InstallationMode -> IO ()
removeBinaryCache uri name (Install ncl) = do
  contents <- NixConf.readWithDefault ncl
  let (final, removed) = NixConf.remove uri name [contents] contents
  NixConf.write ncl final
  filename <- NixConf.getFilename ncl
  if removed
    then putStrLn $ "Removed " <> host <> " binary cache in " <> toS filename
    else putStrLn $ "No " <> host <> " binary cache found in " <> toS filename
  where
    host = URI.toByteString (URI.appendSubdomain name uri)
removeBinaryCache _ _ _ = do
  throwIO $ RemoveCacheUnsupported "Removing binary caches is only supported for nix.conf"

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
  NetRc.add authToken [bc] netrcfile
  putErrText $ "Configured private read access credentials in " <> toS filename
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
