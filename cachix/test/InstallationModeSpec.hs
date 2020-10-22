module InstallationModeSpec where

import Cachix.Client.InstallationMode
import qualified Cachix.Client.NixConf as NixConf
import Protolude
import Test.Hspec

defautUseOptions :: UseOptions
defautUseOptions =
  UseOptions
    { useMode = Nothing,
      useOutputDirectory = Nothing,
      useNixOSFolder = "/etc/nixos"
    }

spec :: Spec
spec = describe "getInstallationMode" $ do
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
