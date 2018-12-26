{-# LANGUAGE QuasiQuotes #-}
module Cachix.Client.InstallationMode
  ( InstallationMode(..)
  , NixEnv(..)
  , getInstallationMode
  , addBinaryCache
  , isTrustedUser
  , getUser
  ) where

import           Protolude
import           Data.String.Here
import qualified Data.Text as T
import           Cachix.Client.Config (Config)
import           Cachix.Client.Exception (CachixException(..))
import qualified Cachix.Client.NetRc as NetRc
import qualified Cachix.Client.NixConf as NixConf
import           Cachix.Client.NixVersion ( NixVersion(..) )
import           Cachix.Api as Api
import           System.Directory               ( getPermissions, writable )
import           System.Environment             ( lookupEnv )
import           System.FilePath                ( replaceFileName )


data NixEnv = NixEnv
  { nixVersion :: NixVersion
  , isTrusted :: Bool
  , isRoot :: Bool
  , isNixOS :: Bool
  }

data InstallationMode
  = Install NixVersion NixConf.NixConfLoc
  | EchoNixOS NixVersion
  | EchoNixOSWithTrustedUser NixVersion
  | UntrustedRequiresSudo
  | Nix20RequiresSudo
  deriving (Show, Eq)

getInstallationMode :: NixEnv -> InstallationMode
getInstallationMode NixEnv{..}
  | isNixOS && (isRoot || nixVersion /= Nix201) = EchoNixOS nixVersion
  | isNixOS && not isTrusted = EchoNixOSWithTrustedUser nixVersion
  | not isNixOS && isRoot = Install nixVersion NixConf.Global
  | nixVersion /= Nix201 = Nix20RequiresSudo
  | isTrusted = Install nixVersion NixConf.Local
  | otherwise = UntrustedRequiresSudo


-- | Add a Binary cache to nix.conf, print nixos config or fail
addBinaryCache :: Maybe Config -> Api.BinaryCache -> InstallationMode -> IO ()
addBinaryCache _ Api.BinaryCache{..} (EchoNixOS _) = do
  putText [iTrim|
nix = {
  binaryCaches = [
    "https://cache.nixos.org/"
    "${uri}"
  ];
  binaryCachePublicKeys = [
    ${T.intercalate " " (map (\s -> "\"" <> s <> "\"") publicSigningKeys)}
  ];
};
  |]
  throwIO $ NixOSInstructions "Add above lines to your NixOS configuration file"
addBinaryCache _ Api.BinaryCache{..} (EchoNixOSWithTrustedUser _) = do
  -- TODO: DRY
  user <- getUser
  putText [iTrim|
nix = {
  binaryCaches = [
    "https://cache.nixos.org/"
    "${uri}"
  ];
  binaryCachePublicKeys = [
    ${T.intercalate " " (map (\s -> "\"" <> s <> "\"") publicSigningKeys)}
  ];
  trustedUsers = [ "root" "${user}" ];
};
  |]
  throwIO $ NixOSInstructions "Add above lines to your NixOS configuration file"
addBinaryCache _ _ UntrustedRequiresSudo = throwIO $
  MustBeRoot "Run command as root OR execute: $ echo \"trusted-users = root $USER\" | sudo tee -a /etc/nix/nix.conf && sudo pkill nix-daemon"
addBinaryCache _ _ Nix20RequiresSudo = throwIO $
  MustBeRoot "Run command as root OR upgrade to latest Nix - to be able to use it without root (recommended)"
addBinaryCache maybeConfig bc@Api.BinaryCache{..} (Install nixversion ncl) = do
  -- TODO: might need locking one day
  gnc <- NixConf.read NixConf.Global
  (input, output) <- case ncl of
    NixConf.Global -> return ([gnc], gnc)
    NixConf.Local -> do
      lnc <- NixConf.read NixConf.Local
      return ([gnc, lnc], lnc)
  let nixconf = fromMaybe (NixConf.NixConf []) output
  NixConf.write nixversion ncl $ NixConf.add bc (catMaybes input) nixconf
  filename <- NixConf.getFilename ncl
  case maybeConfig of
    Nothing -> return () -- no authentication, which is fine for public binary caches
    Just config -> do
      let netrcfile = fromMaybe (replaceFileName filename "netrc" ) Nothing -- TODO: get netrc from nixconf
      NetRc.add config [bc] netrcfile
  putStrLn $ "Configured " <> uri <> " binary cache in " <> toS filename

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
