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

import qualified Cachix.Api as Api
import Cachix.Client.Config (Config)
import Cachix.Client.Exception (CachixException (..))
import qualified Cachix.Client.NetRc as NetRc
import qualified Cachix.Client.NixConf as NixConf
import Data.String.Here
import qualified Data.Text as T
import Protolude
import System.Directory (Permissions, createDirectoryIfMissing, getPermissions, writable)
import System.Environment (lookupEnv)
import System.FilePath ((</>), replaceFileName)
import Prelude (String)

data NixEnv
  = NixEnv
      { isTrusted :: Bool,
        isRoot :: Bool,
        isNixOS :: Bool
      }

-- NOTE: update the list of options for --mode argument in OptionsParser.hs
data InstallationMode
  = Install NixConf.NixConfLoc
  | WriteNixOS
  | UntrustedRequiresSudo
  deriving (Show, Eq)

data UseOptions
  = UseOptions
      { useMode :: Maybe InstallationMode,
        useNixOSFolder :: FilePath
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
toString WriteNixOS = "nixos"
toString UntrustedRequiresSudo = "untrusted-requires-sudo"

getInstallationMode :: NixEnv -> InstallationMode
getInstallationMode nixenv
  | isNixOS nixenv && isRoot nixenv = WriteNixOS
  | not (isNixOS nixenv) && isRoot nixenv = Install NixConf.Global
  | isTrusted nixenv = Install NixConf.Local
  | otherwise = UntrustedRequiresSudo

-- | Add a Binary cache to nix.conf, print nixos config or fail
addBinaryCache :: Maybe Config -> Api.BinaryCache -> UseOptions -> InstallationMode -> IO ()
addBinaryCache _ _ _ UntrustedRequiresSudo =
  throwIO $
    MustBeRoot "Run command as root OR execute: $ echo 'trusted-users = root $USER' | sudo tee -a /etc/nix/nix.conf && sudo pkill nix-daemon"
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
  let nixconf = fromMaybe (NixConf.NixConf []) output
  netrcLocMaybe <- forM (guard $ not (Api.isPublic bc)) $ const $ addPrivateBinaryCacheNetRC maybeConfig bc ncl
  let addNetRCLine :: NixConf.NixConf -> NixConf.NixConf
      addNetRCLine = fromMaybe identity $ do
        netrcLoc <- netrcLocMaybe :: Maybe FilePath
        -- We only add the netrc line for local user configs for now.
        -- On NixOS we assume it will be picked up from the default location.
        guard (ncl == NixConf.Local)
        pure (NixConf.setNetRC $ toS netrcLoc)
  NixConf.write ncl $ addNetRCLine $ NixConf.add bc (catMaybes input) nixconf
  filename <- NixConf.getFilename ncl
  putStrLn $ "Configured " <> Api.uri bc <> " binary cache in " <> toS filename

nixosBinaryCache :: Maybe Config -> Api.BinaryCache -> UseOptions -> IO ()
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
      unless (Api.isPublic bc) $ void $ addPrivateBinaryCacheNetRC maybeConfig bc NixConf.Global
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
    cacheModuleFile = toplevel <> "/" <> toS (Api.name bc) <> ".nix"
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
Binary cache ${Api.name bc} configuration written to ${cacheModuleFile}.

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
  nix.binaryCaches = ["https://cache.nixos.org/"];
}
    |]
    cacheModule :: Text
    cacheModule =
      [i|
{
  nix = {
    binaryCaches = [
      "${Api.uri bc}"
    ];
    binaryCachePublicKeys = [
      ${T.intercalate " " (map (\s -> "\"" <> s <> "\"") (Api.publicSigningKeys bc))}
    ];
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
  let isTrustedU = writable permissions || user `elem` users
  when (not (null groups) && not isTrustedU) $ do
    -- TODO: support Nix group syntax
    putText "Warn: cachix doesn't yet support checking if user is trusted via groups, so it will recommend sudo"
    putStrLn $ "Warn: groups found " <> T.intercalate "," groups
  return isTrustedU
  where
    groups = filter (\u -> T.head u == '@') users

getUser :: IO Text
getUser = do
  maybeUser <- lookupEnv "USER"
  case maybeUser of
    Nothing -> throwIO $ UserEnvNotSet "$USER must be set"
    Just user -> return $ toS user
