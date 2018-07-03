module InstallationModeSpec where

import Protolude
import Test.Hspec

import qualified Cachix.Client.NixConf as NixConf
import           Cachix.Client.NixVersion ( NixMode(..) )
import           Cachix.Client.InstallationMode


spec :: Spec
spec = describe "getInstallationMode" $ do
  it "Nix 1.X is unsupported" $
    let
      nixenv = NixEnv
        { nixMode = Nix1XX
        , isTrusted = False -- any
        , isRoot = False -- any
        , isNixOS = False -- any
        }
    in getInstallationMode nixenv `shouldBe` UnsupportedNix1X
  it "NixOS with root prints configuration" $
    let
      nixenv = NixEnv
        { nixMode = Nix20 -- any except 1.0
        , isTrusted = True -- any
        , isRoot = True
        , isNixOS = True
        }
    in getInstallationMode nixenv `shouldBe` EchoNixOS
  it "NixOS without trust prints configuration plus trust" $
    let
      nixenv = NixEnv
        { nixMode = Nix20
        , isTrusted = False
        , isRoot = False
        , isNixOS = True
        }
    in getInstallationMode nixenv `shouldBe` EchoNixOSWithTrustedUser
  it "NixOS non-root trusted results into local install" $
    let
      nixenv = NixEnv
        { nixMode = Nix201
        , isTrusted = True
        , isRoot = False
        , isNixOS = True
        }
    in getInstallationMode nixenv `shouldBe` Install NixConf.Local
  it "non-NixOS root results into global install" $
    let
      nixenv = NixEnv
        { nixMode = Nix201
        , isTrusted = True
        , isRoot = True
        , isNixOS = False
        }
    in getInstallationMode nixenv `shouldBe` Install NixConf.Global
  it "non-NixOS non-root trusted results into local install" $
    let
      nixenv = NixEnv
        { nixMode = Nix201
        , isTrusted = True
        , isRoot = False
        , isNixOS = False
        }
    in getInstallationMode nixenv `shouldBe` Install NixConf.Local
  it "non-NixOS non-root non-trusted results into required sudo" $
    let
      nixenv = NixEnv
        { nixMode = Nix201
        , isTrusted = False
        , isRoot = False
        , isNixOS = False
        }
    in getInstallationMode nixenv `shouldBe` UntrustedRequiresSudo
  it "non-NixOS non-root non-trusted on Nix 2.0.0 results into required sudo with upgrade warning" $
    let
      nixenv = NixEnv
        { nixMode = Nix20
        , isTrusted = False
        , isRoot = False
        , isNixOS = False
        }
    in getInstallationMode nixenv `shouldBe` Nix20RequiresSudo
  it "non-NixOS non-root trusted on Nix 2.0.0 results into required sudo with upgrade warning" $
    let
      nixenv = NixEnv
        { nixMode = Nix20
        , isTrusted = True
        , isRoot = False
        , isNixOS = False
        }
    in getInstallationMode nixenv `shouldBe` Nix20RequiresSudo
