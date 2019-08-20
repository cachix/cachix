module NixVersionSpec where

import Cachix.Client.NixVersion
import Protolude
import Test.Hspec

spec :: Spec
spec = describe "parseNixVersion" $ do
  it "parses 'nix-env (Nix) 2.0' as Nix20"
    $ parseNixVersion "nix-env (Nix) 2.0"
    `shouldBe` Right Nix20
  it "parses 'nix-env (Nix) 2.0.1' as Nix201"
    $ parseNixVersion "nix-env (Nix) 2.0.1"
    `shouldBe` Right Nix201
  it "parses 'nix-env (Nix) 1.11.13' as Nix1XX"
    $ parseNixVersion "nix-env (Nix) 1.11.13"
    `shouldBe` Right Nix1XX
  it "parses 'nix-env (Nix) 2.0.5' as Nix201"
    $ parseNixVersion "nix-env (Nix) 2.0.5"
    `shouldBe` Right Nix201
  it "parses 'nix-env (Nix) 2.0.1pre6053_444b921' as Nix201"
    $ parseNixVersion "nix-env (Nix) 2.0.1pre6053_444b921"
    `shouldBe` Right Nix201
  it "fails with unknown string 'foobar'"
    $ parseNixVersion "foobar"
    `shouldBe` Left "Couldn't parse 'nix-env --version' output: foobar"
