module InstallationModeSpec where

import Cachix.Client.Config (Config)
import Cachix.Client.InstallationMode
import Cachix.Client.NixConf qualified as NixConf
import Cachix.Types.BinaryCache (BinaryCache (..), CompressionMethod (..))
import Cachix.Types.Permission (Permission (..))
import Protolude
import System.Directory (createDirectoryIfMissing, doesFileExist, getPermissions, setOwnerWritable, setPermissions)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import Test.Hspec

defautUseOptions :: UseOptions
defautUseOptions =
  UseOptions
    { useMode = Nothing,
      useOutputDirectory = Nothing,
      useNixOSFolder = "/etc/nixos"
    }

publicCache :: BinaryCache
publicCache =
  BinaryCache
    { name = "name",
      uri = "https://name.cachix.org",
      isPublic = True,
      permission = Admin,
      publicSigningKeys = ["name.cachix.org-1:pub"],
      githubUsername = "foobar",
      preferredCompressionMethod = ZSTD
    }

-- | Config is only consulted for private caches (netrc credentials), which
-- these tests do not exercise.
unusedConfig :: Config
unusedConfig = panic "Config is not used for public caches"

-- | Run an action against a temp directory serving as XDG_CONFIG_HOME, so
-- the Local install mode operates on <tmp>/nix/nix.conf.
withLocalNixConf :: (FilePath -> IO a) -> IO a
withLocalNixConf action =
  withTempDirectory "/tmp" "cachix-install" $ \temp -> do
    setEnv "XDG_CONFIG_HOME" temp
    action (temp </> "nix")

useLocal :: IO ()
useLocal = addBinaryCache unusedConfig publicCache defautUseOptions (Install NixConf.Local)

removeLocal :: IO ()
removeLocal = removeBinaryCache "https://cachix.org" "name" (Install NixConf.Local)

makeReadOnly :: FilePath -> IO ()
makeReadOnly path = do
  permissions <- getPermissions path
  setPermissions path (setOwnerWritable False permissions)

spec :: Spec
spec = do
  describe "addBinaryCache and removeBinaryCache" $ do
    it "writes the fragment and includes it from nix.conf" $
      withLocalNixConf $ \dir -> do
        useLocal
        readFile (dir </> "nix.conf") `shouldReturn` "!include cachix.conf\n"
        readFile (dir </> "cachix.conf")
          `shouldReturn` "extra-substituters = https://name.cachix.org\nextra-trusted-public-keys = name.cachix.org-1:pub\n"

    it "is idempotent across runs" $
      withLocalNixConf $ \dir -> do
        useLocal
        nixConf <- readFile (dir </> "nix.conf")
        fragment <- readFile (dir </> "cachix.conf")
        useLocal
        readFile (dir </> "nix.conf") `shouldReturn` nixConf
        readFile (dir </> "cachix.conf") `shouldReturn` fragment

    it "migrates legacy inline settings into the fragment" $
      withLocalNixConf $ \dir -> do
        createDirectoryIfMissing True dir
        writeFile (dir </> "nix.conf") $
          unlines
            [ "trusted-users = root",
              "substituters = " <> NixConf.defaultPublicURI <> " https://old.cachix.org",
              "trusted-public-keys = " <> NixConf.defaultSigningKey <> " old-key"
            ]
        useLocal
        readFile (dir </> "nix.conf") `shouldReturn` "trusted-users = root\n!include cachix.conf\n"
        fragment <- readFile (dir </> "cachix.conf")
        fragment
          `shouldBe` unlines
            [ "extra-substituters = " <> NixConf.defaultPublicURI <> " https://old.cachix.org https://name.cachix.org",
              "extra-trusted-public-keys = " <> NixConf.defaultSigningKey <> " old-key name.cachix.org-1:pub"
            ]

    it "succeeds against a read-only nix.conf that already has the include" $
      withLocalNixConf $ \dir -> do
        createDirectoryIfMissing True dir
        let legacyContents =
              unlines
                [ "substituters = " <> NixConf.defaultPublicURI <> " https://old.cachix.org",
                  "trusted-public-keys = " <> NixConf.defaultSigningKey <> " old-key",
                  "!include cachix.conf"
                ]
        writeFile (dir </> "nix.conf") legacyContents
        makeReadOnly (dir </> "nix.conf")
        useLocal
        readFile (dir </> "nix.conf") `shouldReturn` legacyContents

    it "removes the cache from the fragment and keeps the include" $
      withLocalNixConf $ \dir -> do
        useLocal
        removeLocal
        readFile (dir </> "nix.conf") `shouldReturn` "!include cachix.conf\n"
        readFile (dir </> "cachix.conf") `shouldReturn` ""

    it "does not add an include to nix.conf as part of a removal" $
      withLocalNixConf $ \dir -> do
        createDirectoryIfMissing True dir
        writeFile (dir </> "cachix.conf") $
          unlines
            [ "extra-substituters = https://other.cachix.org https://name.cachix.org",
              "extra-trusted-public-keys = other-key name.cachix.org-1:key"
            ]
        removeLocal
        doesFileExist (dir </> "nix.conf") `shouldReturn` False
        readFile (dir </> "cachix.conf")
          `shouldReturn` "extra-substituters = https://other.cachix.org\nextra-trusted-public-keys = other-key\n"

    it "writes and removes a self-contained nix.conf for an output directory" $
      withTempDirectory "/tmp" "cachix-standalone" $ \temp -> do
        addBinaryCache unusedConfig publicCache defautUseOptions (Install (NixConf.Custom temp))
        readFile (temp </> "nix.conf")
          `shouldReturn` "extra-substituters = https://name.cachix.org\nextra-trusted-public-keys = name.cachix.org-1:pub\n"
        removeBinaryCache "https://cachix.org" "name" (Install (NixConf.Custom temp))
        readFile (temp </> "nix.conf") `shouldReturn` ""

  describe "getInstallationMode" $ do
    it "NixOS with root prints configuration" $
      let nixenv =
            NixEnv
              { isTrusted = True, -- any
                isRoot = True,
                isNixOS = True
              }
       in getInstallationMode nixenv defautUseOptions `shouldBe` WriteNixOS
    it "NixOS without trust prints steps to follow" $
      let nixenv =
            NixEnv
              { isTrusted = False,
                isRoot = False,
                isNixOS = True
              }
       in getInstallationMode nixenv defautUseOptions `shouldBe` UntrustedNixOS
    it "NixOS without trust prints steps to follow" $
      let nixenv =
            NixEnv
              { isTrusted = False,
                isRoot = False,
                isNixOS = True
              }
       in getInstallationMode nixenv defautUseOptions `shouldBe` UntrustedNixOS
    it "NixOS non-root trusted results into local install" $
      let nixenv =
            NixEnv
              { isTrusted = True,
                isRoot = False,
                isNixOS = True
              }
       in getInstallationMode nixenv defautUseOptions `shouldBe` Install NixConf.Local
    it "non-NixOS root results into global install" $
      let nixenv =
            NixEnv
              { isTrusted = True,
                isRoot = True,
                isNixOS = False
              }
       in getInstallationMode nixenv defautUseOptions `shouldBe` Install NixConf.Global
    it "non-NixOS with Nix 1.X root results into global install" $
      let nixenv =
            NixEnv
              { isTrusted = True,
                isRoot = True,
                isNixOS = False -- any
              }
       in getInstallationMode nixenv defautUseOptions `shouldBe` Install NixConf.Global
    it "non-NixOS non-root trusted results into local install" $
      let nixenv =
            NixEnv
              { isTrusted = True,
                isRoot = False,
                isNixOS = False
              }
       in getInstallationMode nixenv defautUseOptions `shouldBe` Install NixConf.Local
    it "non-NixOS non-root non-trusted results into required sudo" $
      let nixenv =
            NixEnv
              { isTrusted = False,
                isRoot = False,
                isNixOS = False
              }
       in getInstallationMode nixenv defautUseOptions `shouldBe` UntrustedRequiresSudo
