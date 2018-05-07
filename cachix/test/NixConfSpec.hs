{-# LANGUAGE QuasiQuotes #-}
module NixConfSpec where

import Protolude
import Data.String.Here
import Test.Hspec

import Cachix.Client.NixConf as NixConf


property :: Text -> Expectation
property x = NixConf.render <$> parse x `shouldBe` Just x

spec :: Spec
spec = do
  describe "render . parse" $ do
    it "handles single value substituters" $
      property "substituters = a\n"
    it "handles multi value substituters" $
      property "substituters = a b c\n"
    it "handles all known keys" $
      property "substituters = a b c\ntrusted-users = him me\ntrusted-public-keys = a\n"
    it "random content" $
      property "blabla = foobar\nfoo = bar\n"
  describe "add" $ do
    it "is idempotent" $
      let
        x = Substituters ["a"]
      in add x "a" `shouldBe` x
    it "basic case" $
      add (Substituters ["a"]) "b" `shouldBe` Substituters ["b", "a"]
  describe "parse" $ do
    it "parses substituters" $
      parse "substituters = a\n" `shouldBe` (Just $ NixConf [Substituters ["a"]])
    it "parses substituters with multiple values" $
      parse "substituters = a b c\n" `shouldBe` (Just $ NixConf [Substituters ["a", "b", "c"]])
    it "parses a complex example" $
      parse realExample `shouldBe` (Just $ NixConf  [ Other ""
                                                    , Substituters ["a","b","c"]
                                                    , TrustedUsers ["him","me"]
                                                    , TrustedPublicKeys ["a"]
                                                    , Other "blabla =  asd"
                                                    , Other "# comment"
                                                    , Other ""
                                                    , Other ""])

realExample = [hereLit|
substituters = a  b c
trusted-users = him me
trusted-public-keys  =  a
blabla =  asd
# comment


|]
