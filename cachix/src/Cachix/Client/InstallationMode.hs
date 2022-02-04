{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.InstallationMode
  ( InstallationMode (..),
    NixEnv (..),
    getInstallationMode,
    addBinaryCache,
    isTrustedUser,
    getUser,
    fromString,
    toString,
    UseOptions (..),
  )
where

import Cachix.Client.Config (Config)
import qualified Cachix.Client.Config as Config
import Cachix.Client.Exception (CachixException (..))
import qualified Cachix.Client.NetRc as NetRc
import qualified Cachix.Client.NixConf as NixConf
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Data.Maybe
import Data.String.Here
import qualified Data.Text as T
import Protolude
import System.Directory (Permissions, createDirectoryIfMissing, getPermissions, writable)
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
addBinaryCache :: Maybe Config -> BinaryCache.BinaryCache -> UseOptions -> InstallationMode -> IO ()
addBinaryCache _ _ _ UntrustedNixOS = do
  user <- getUser
  throwIO $
    MustBeRoot
      [i|This user doesn't have permissions to configure binary caches.

You can either:

a) Run the same command as root to write NixOS configuration.

b) Add the following to your configuration.nix to add your user as trusted 
   and then try again:

  nix.trustedUsers = [ "root" "${user}" ];

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
addBinaryCache maybeConfig bc useOptions WriteNixOS =
  nixosBinaryCache maybeConfig bc useOptions
addBinaryCache maybeConfig bc _ (Install ncl) = do
  -- TODO: might need locking one day
  gnc <- NixConf.read NixConf.Global
  (input, output) <-
    case ncl of
      NixConf.Global -> return ([gnc], gnc)
      NixConf.Local -> do
        lnc <- NixConf.read NixConf.Local
        return ([gnc, lnc], lnc)
      NixConf.Custom _ -> do
        lnc <- NixConf.read ncl
        return ([lnc], lnc)
  let nixconf = fromMaybe (NixConf.NixConf []) output
  netrcLocMaybe <- forM (guard $ not (BinaryCache.isPublic bc)) $ const $ addPrivateBinaryCacheNetRC maybeConfig bc ncl
  let addNetRCLine :: NixConf.NixConf -> NixConf.NixConf
      addNetRCLine = fromMaybe identity $ do
        netrcLoc <- netrcLocMaybe :: Maybe FilePath
        -- We only add the netrc line for local user configs for now.
        -- On NixOS we assume it will be picked up from the default location.
        guard (ncl == NixConf.Local)
        pure (NixConf.setNetRC $ toS netrcLoc)
  NixConf.write ncl $ addNetRCLine $ NixConf.add bc (catMaybes input) nixconf
  filename <- NixConf.getFilename ncl
  putStrLn $ "Configured " <> BinaryCache.uri bc <> " binary cache in " <> toS filename

nixosBinaryCache :: Maybe Config -> BinaryCache.BinaryCache -> UseOptions -> IO ()
nixosBinaryCache maybeConfig bc UseOptions {useNixOSFolder = baseDirectory} = do
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
      unless (BinaryCache.isPublic bc) $ void $ addPrivateBinaryCacheNetRC maybeConfig bc NixConf.Global
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
addPrivateBinaryCacheNetRC :: Maybe Config -> BinaryCache.BinaryCache -> NixConf.NixConfLoc -> IO FilePath
addPrivateBinaryCacheNetRC maybeConfig bc nixconf = do
  filename <- (`replaceFileName` "netrc") <$> NixConf.getFilename nixconf
  authToken <- Config.getAuthTokenRequired maybeConfig
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
