{-# LANGUAGE QuasiQuotes #-}
module NixConfSpec where

import Protolude
import Data.String.Here
import Test.Hspec

import Cachix.Client.NixConf as NixConf
import Cachix.Client.NixVersion (NixVersion(..))
import Cachix.Api (BinaryCache(..))


property :: Text -> Expectation
property x = NixConf.render Nix20 <$> parse x `shouldBe` Right x

propertyNix1 :: Text -> Expectation
propertyNix1 x = NixConf.render Nix1XX <$> parse x `shouldBe` Right x

bc = BinaryCache
 { name = "name"
 , uri = "https://name.cachix.org"
 , publicSigningKeys = ["pub"]
 }

spec :: Spec
spec = do
  describe "render . parse" $ do
    it "handles single value substituters" $
      property "substituters = a\n"
    it "handles multi value substituters" $
      property "substituters = a b c\n"
    it "handles all known keys" $
      property "substituters = a b c\ntrusted-users = him me\ntrusted-public-keys = a\n"
    it "handles all known keys for Nix 1.0" $
      propertyNix1 "binary-caches = a b c\ntrusted-users = him me\nbinary-cache-public-keys = a\n"
    it "random content" $
      property "blabla = foobar\nfoo = bar\n"
  describe "add" $ do
    it "merges binary caches from both files" $
      let
        globalConf = NixConf
          [ Substituters ["bc1"]
          , TrustedPublicKeys ["pub1"]
          ]
        localConf = NixConf
          [ Substituters ["bc2"]
          , TrustedPublicKeys ["pub2"]
          ]
        result = NixConf
          [ Substituters [defaultPublicURI, "bc1", "bc2", "https://name.cachix.org"]
          , TrustedPublicKeys [defaultSigningKey, "pub1", "pub2", "pub"]
          ]
      in add bc [globalConf, localConf] localConf `shouldBe` result
    it "is noop if binary cache exists in one file" $
      let
        globalConf = NixConf
          [ Substituters [defaultPublicURI, "https://name.cachix.org"]
          , TrustedPublicKeys [defaultSigningKey, "pub"]
          ]
        localConf = NixConf [ ]
        result = NixConf
          [ Substituters [defaultPublicURI, "https://name.cachix.org"]
          , TrustedPublicKeys [defaultSigningKey, "pub"]
          ]
      in add bc [globalConf, localConf] localConf `shouldBe` result
    it "preserves other nixconf entries" $
      let
        globalConf = NixConf
          [ Substituters ["http"]
          , TrustedPublicKeys ["pub1"]
          , TrustedUsers ["user"]
          , Other "foo"
          ]
        localConf = NixConf
          [ TrustedUsers ["user2"]
          , Other "bar"
          ]
        result = NixConf
          [ TrustedUsers ["user2"]
          , Other "bar"
          , Substituters [defaultPublicURI, "http", "https://name.cachix.org"]
          , TrustedPublicKeys [defaultSigningKey, "pub1", "pub"]
          ]
      in add bc [globalConf, localConf] localConf `shouldBe` result
    it "removed duplicates" $
      let
        globalConf = NixConf
          [ Substituters ["bc1", "bc1"]
          , TrustedPublicKeys ["pub1", "pub1"]
          ]
        localConf = NixConf
          [ Substituters ["bc2", "bc2"]
          , TrustedPublicKeys ["pub2", "pub2"]
          ]
        result = NixConf
          [ Substituters [defaultPublicURI, "bc1", "bc2", "https://name.cachix.org"]
          , TrustedPublicKeys [defaultSigningKey, "pub1", "pub2", "pub"]
          ]
      in add bc [globalConf, localConf] localConf `shouldBe` result
    it "adds binary cache and defaults if no existing entries exist" $
      let
        globalConf = NixConf []
        localConf = NixConf []
        result = NixConf
          [ Substituters [defaultPublicURI, "https://name.cachix.org"]
          , TrustedPublicKeys [defaultSigningKey, "pub"]
          ]
      in add bc [globalConf, localConf] localConf `shouldBe` result
  describe "parse" $ do
    it "parses substituters" $
      parse "substituters = a\n" `shouldBe` (Right $ NixConf [Substituters ["a"]])
    it "parses long key" $
      parse "binary-caches-parallel-connections = 40\n" `shouldBe` (Right $ NixConf [Other "binary-caches-parallel-connections = 40"])
    it "parses substituters with multiple values" $
      parse "substituters = a b c\n" `shouldBe` (Right $ NixConf [Substituters ["a", "b", "c"]])
    it "parses equal sign after the first key as literal" $
      parse "substituters = a b c= d\n" `shouldBe` (Right $ NixConf [Substituters ["a", "b", "c=", "d"]])
    it "parses with missing endline" $
      parse "allowed-users = *"  `shouldBe` (Right $ NixConf [Other "allowed-users = *"])
    it "parses a complex example" $
      parse realExample `shouldBe` (Right $ NixConf [ Other ""
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
