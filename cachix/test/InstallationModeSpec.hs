module InstallationModeSpec where

import Protolude
import Test.Hspec

import qualified Cachix.Client.NixConf as NixConf
import           Cachix.Client.NixVersion ( NixVersion(..) )
import           Cachix.Client.InstallationMode


spec :: Spec
spec = describe "getInstallationMode" $ do
  it "NixOS with root prints configuration" $
    let
      nixenv = NixEnv
        { nixVersion = Nix20 -- any except 1.0
        , isTrusted = True -- any
        , isRoot = True
        , isNixOS = True
        }
    in getInstallationMode nixenv `shouldBe` EchoNixOS Nix20
  it "NixOS without trust prints configuration plus trust" $
    let
      nixenv = NixEnv
        { nixVersion = Nix201
        , isTrusted = False
        , isRoot = False
        , isNixOS = True
        }
    in getInstallationMode nixenv `shouldBe` EchoNixOSWithTrustedUser Nix201
  it "NixOS without trust prints configuration on older Nix" $
    let
      nixenv = NixEnv
        { nixVersion = Nix20
        , isTrusted = False
        , isRoot = False
        , isNixOS = True
        }
    in getInstallationMode nixenv `shouldBe` EchoNixOS Nix20
  it "NixOS non-root trusted results into local install" $
    let
      nixenv = NixEnv
        { nixVersion = Nix201
        , isTrusted = True
        , isRoot = False
        , isNixOS = True
        }
    in getInstallationMode nixenv `shouldBe` Install Nix201 NixConf.Local
  it "non-NixOS root results into global install" $
    let
      nixenv = NixEnv
        { nixVersion = Nix201
        , isTrusted = True
        , isRoot = True
        , isNixOS = False
        }
    in getInstallationMode nixenv `shouldBe` Install Nix201 NixConf.Global
  it "non-NixOS with Nix 1.X root results into global install" $
      let
        nixenv = NixEnv
          { nixVersion = Nix1XX
          , isTrusted = True
          , isRoot = True
          , isNixOS = False -- any
          }
      in getInstallationMode nixenv `shouldBe` Install Nix1XX NixConf.Global
  it "non-NixOS non-root trusted results into local install" $
    let
      nixenv = NixEnv
        { nixVersion = Nix201
        , isTrusted = True
        , isRoot = False
        , isNixOS = False
        }
    in getInstallationMode nixenv `shouldBe` Install Nix201 NixConf.Local
  it "non-NixOS non-root non-trusted results into required sudo" $
    let
      nixenv = NixEnv
        { nixVersion = Nix201
        , isTrusted = False
        , isRoot = False
        , isNixOS = False
        }
    in getInstallationMode nixenv `shouldBe` UntrustedRequiresSudo
  it "non-NixOS non-root non-trusted on Nix 2.0.0 results into required sudo with upgrade warning" $
    let
      nixenv = NixEnv
        { nixVersion = Nix20
        , isTrusted = False
        , isRoot = False
        , isNixOS = False
        }
    in getInstallationMode nixenv `shouldBe` Nix20RequiresSudo
  it "non-NixOS non-root trusted on Nix 2.0.0 results into required sudo with upgrade warning" $
    let
      nixenv = NixEnv
        { nixVersion = Nix20
        , isTrusted = True
        , isRoot = False
        , isNixOS = False
        }
    in getInstallationMode nixenv `shouldBe` Nix20RequiresSudo
