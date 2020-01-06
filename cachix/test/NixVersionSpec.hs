module NixVersionSpec where

import Cachix.Client.NixVersion
import Protolude
import Test.Hspec

spec :: Spec
spec = describe "parseNixVersion" $ do
  it "parses 'nix-env (Nix) 2.0' as Nix20" $
    parseNixVersion "nix-env (Nix) 2.0"
      `shouldBe` Left "Nix 2.0.2 or lower is not supported. Please upgrade: https://nixos.org/nix/"
  it "parses 'nix-env (Nix) 2.0.1' as Nix201" $
    parseNixVersion "nix-env (Nix) 2.0.1"
      `shouldBe` Right ()
  it "parses 'nix-env (Nix) 1.11.13' as Nix1XX" $
    parseNixVersion "nix-env (Nix) 1.11.13"
      `shouldBe` Left "Nix 2.0.2 or lower is not supported. Please upgrade: https://nixos.org/nix/"
  it "parses 'nix-env (Nix) 2.0.5' as Nix201" $
    parseNixVersion "nix-env (Nix) 2.0.5"
      `shouldBe` Right ()
  it "parses 'nix-env (Nix) 2.0.1pre6053_444b921' as Nix201" $
    parseNixVersion "nix-env (Nix) 2.0.1pre6053_444b921"
      `shouldBe` Right ()
  it "fails with unknown string 'foobar'" $
    parseNixVersion "foobar"
      `shouldBe` Left "Couldn't parse 'nix-env --version' output: foobar"
