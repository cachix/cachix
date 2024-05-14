module NixVersionSpec (spec) where

import Cachix.Client.NixVersion
import Data.Versions (Versioning, versioning)
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "parseNixVersion" $ do
    it "fails with unknown string 'foobar'" $
      parseNixVersion "foobar" `shouldSatisfy` isLeft

    it "parses out a semver out of an unexpected string" $
      parseNixVersion "foobar 2.0.1 other stuff"
        `shouldBe` Right (toVersioning "2.0.1")

    describe "Nix" $
      runParserTests
        [ ("nix-env (Nix) 2.0", toVersioning "2.0"),
          ("nix-env (Nix) 2.0.1", toVersioning "2.0.1"),
          ("nix-env (Nix) 2.0.1\n", toVersioning "2.0.1")
        ]

    describe "Lix" $
      runParserTests
        [ ("nix-env (Lix, like Nix) 2.90-beta.0", toVersioning "2.90-beta.0"),
          ("nix-env (Lix, like Nix) 2.90-beta.1-lixpre20240506-b6799ab", toVersioning "2.90-beta.1-lixpre20240506-b6799ab")
        ]

runParserTests :: [(Text, Versioning)] -> Spec
runParserTests tests = do
  forM_ tests $ \(input, expected) ->
    it ("parses '" <> toS input <> "'") $
      parseNixVersion input `shouldBe` Right expected

toVersioning :: Text -> Versioning
toVersioning str =
  versioning str & fromRight (panic "Couldn't parse version")
