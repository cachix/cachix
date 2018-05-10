{-# LANGUAGE QuasiQuotes #-}
module NixConfSpec where

import Protolude
import Data.String.Here
import Test.Hspec

import Cachix.Client.NixConf as NixConf
import Cachix.Api (BinaryCache(..))


property :: Text -> Expectation
property x = NixConf.render <$> parse x `shouldBe` Just x

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
          [ Substituters ["bc1"]
          , BinaryCaches ["bc1"]
          , TrustedPublicKeys ["pub1"]
          , BinaryCachePublicKeys ["pub1"]
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
    it "can mix and match old and new names" $
      let
        globalConf = NixConf
          [ BinaryCaches ["bc1"]
          , BinaryCachePublicKeys ["pub1"]
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
