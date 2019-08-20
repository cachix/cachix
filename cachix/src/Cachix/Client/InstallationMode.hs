{-# LANGUAGE QuasiQuotes #-}

module Cachix.Client.InstallationMode
  ( InstallationMode (..),
    NixEnv (..),
    getInstallationMode,
    addBinaryCache,
    isTrustedUser,
    getUser
    )
where

import Cachix.Api as Api
import Cachix.Client.Config (Config)
import Cachix.Client.Exception (CachixException (..))
import qualified Cachix.Client.NetRc as NetRc
import qualified Cachix.Client.NixConf as NixConf
import Cachix.Client.NixVersion (NixVersion (..))
import Cachix.Client.OptionsParser (UseOptions (..))
import Data.String.Here
import qualified Data.Text as T
import Protolude
import System.Directory (createDirectoryIfMissing, getPermissions, writable)
import System.Environment (lookupEnv)
import System.FilePath ((</>), replaceFileName)

data NixEnv
  = NixEnv
      { nixVersion :: NixVersion,
        isTrusted :: Bool,
        isRoot :: Bool,
        isNixOS :: Bool
        }

data InstallationMode
  = Install NixVersion NixConf.NixConfLoc
  | EchoNixOS NixVersion
  | EchoNixOSWithTrustedUser NixVersion
  | UntrustedRequiresSudo
  | Nix20RequiresSudo
  deriving (Show, Eq)

getInstallationMode :: NixEnv -> InstallationMode
getInstallationMode NixEnv {..}
  | isNixOS && (isRoot || nixVersion /= Nix201) = EchoNixOS nixVersion
  | isNixOS && not isTrusted = EchoNixOSWithTrustedUser nixVersion
  | not isNixOS && isRoot = Install nixVersion NixConf.Global
  | nixVersion /= Nix201 = Nix20RequiresSudo
  | isTrusted = Install nixVersion NixConf.Local
  | otherwise = UntrustedRequiresSudo

-- | Add a Binary cache to nix.conf, print nixos config or fail
addBinaryCache :: Maybe Config -> Api.BinaryCache -> UseOptions -> InstallationMode -> IO ()
addBinaryCache _ _ _ UntrustedRequiresSudo =
  throwIO
    $ MustBeRoot "Run command as root OR execute: $ echo \"trusted-users = root $USER\" | sudo tee -a /etc/nix/nix.conf && sudo pkill nix-daemon"
addBinaryCache _ _ _ Nix20RequiresSudo =
  throwIO
    $ MustBeRoot "Run command as root OR upgrade to latest Nix - to be able to use it without root (recommended)"
addBinaryCache maybeConfig bc useOptions (EchoNixOS _) =
  nixosBinaryCache maybeConfig Nothing bc useOptions
addBinaryCache maybeConfig bc useOptions (EchoNixOSWithTrustedUser _) = do
  user <- getUser
  nixosBinaryCache maybeConfig (Just user) bc useOptions
addBinaryCache maybeConfig bc@Api.BinaryCache {..} _ (Install nixversion ncl) = do
  -- TODO: might need locking one day
  gnc <- NixConf.read NixConf.Global
  (input, output) <-
    case ncl of
      NixConf.Global -> return ([gnc], gnc)
      NixConf.Local -> do
        lnc <- NixConf.read NixConf.Local
        return ([gnc, lnc], lnc)
  let nixconf = fromMaybe (NixConf.NixConf []) output
  netrcLocMaybe <- forM (guard $ not isPublic) $ const $ addPrivateBinaryCacheNetRC maybeConfig bc ncl
  let addNetRCLine :: NixConf.NixConf -> NixConf.NixConf
      addNetRCLine = fromMaybe identity $ do
        netrcLoc <- netrcLocMaybe :: Maybe FilePath
        -- We only add the netrc line for local user configs for now.
        -- On NixOS we assume it will be picked up from the default location.
        guard (ncl == NixConf.Local)
        pure (NixConf.setNetRC $ toS netrcLoc)
  NixConf.write nixversion ncl $ addNetRCLine $ NixConf.add bc (catMaybes input) nixconf
  filename <- NixConf.getFilename ncl
  putStrLn $ "Configured " <> uri <> " binary cache in " <> toS filename

nixosBinaryCache :: Maybe Config -> Maybe Text -> Api.BinaryCache -> UseOptions -> IO ()
nixosBinaryCache maybeConfig maybeUser bc@Api.BinaryCache {..} UseOptions {useNixOSFolder = baseDirectory} = do
  createDirectoryIfMissing True $ toS toplevel
  writeFile (toS glueModuleFile) glueModule
  writeFile (toS cacheModuleFile) cacheModule
  unless isPublic $ void $ addPrivateBinaryCacheNetRC maybeConfig bc NixConf.Global
  putText instructions
  where
    configurationNix :: Text
    configurationNix = toS $ toS baseDirectory </> "configuration.nix"
    namespace :: Text
    namespace = "cachix"
    toplevel :: Text
    toplevel = toS $ toS baseDirectory </> toS namespace
    glueModuleFile :: Text
    glueModuleFile = toplevel <> ".nix"
    cacheModuleFile :: Text
    cacheModuleFile = toplevel <> "/" <> toS name <> ".nix"
    extra :: Text
    extra = maybe "" (\user -> [i|trustedUsers = [ "root" "${user}" ];|]) maybeUser
    instructions :: Text
    instructions =
      [iTrim|
Cachix configuration written to ${glueModuleFile}.
Binary cache ${name} configuration written to ${cacheModuleFile}.

To start using cachix add the following to your ${configurationNix}:

    imports = [ ./cachix.nix ];

Then run:

    $ nixos-rebuild switch
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
  nix.binaryCaches = ["https://cache.nixos.org/"];
}
    |]
    cacheModule :: Text
    cacheModule =
      [i|
{
  nix = {
    binaryCaches = [
      "${uri}"
    ];
    binaryCachePublicKeys = [
      ${T.intercalate " " (map (\s -> "\"" <> s <> "\"") publicSigningKeys)}
    ];
    ${extra}
  };
}
    |]

-- TODO: allow overriding netrc location
addPrivateBinaryCacheNetRC :: Maybe Config -> Api.BinaryCache -> NixConf.NixConfLoc -> IO FilePath
addPrivateBinaryCacheNetRC maybeConfig bc nixconf = do
  filename <- (`replaceFileName` "netrc") <$> NixConf.getFilename nixconf
  case maybeConfig of
    Nothing -> panic "Run $ cachix authtoken <token>"
    Just config -> do
      let netrcfile = fromMaybe filename Nothing -- TODO: get netrc from nixconf
      NetRc.add config [bc] netrcfile
      putErrText $ "Configured private read access credentials in " <> toS filename
      pure filename

isTrustedUser :: [Text] -> IO Bool
isTrustedUser users = do
  user <- getUser
  -- to detect single user installations
  permissions <- getPermissions "/nix/store"
  let isTrusted = writable permissions || user `elem` users
  when (not (null groups) && not isTrusted) $ do
    -- TODO: support Nix group syntax
    putText "Warn: cachix doesn't yet support checking if user is trusted via groups, so it will recommend sudo"
    putStrLn $ "Warn: groups found " <> T.intercalate "," groups
  return isTrusted
  where
    groups = filter (\u -> T.head u == '@') users

getUser :: IO Text
getUser = do
  maybeUser <- lookupEnv "USER"
  case maybeUser of
    Nothing -> throwIO $ UserEnvNotSet "$USER must be set"
    Just user -> return $ toS user
