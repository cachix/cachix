module CommandListSpec (spec) where

import Cachix.Client.Command.List (cachixCacheName)
import Cachix.Client.URI (URI)
import Protolude
import Test.Hspec

spec :: Spec
spec =
  describe "cachixCacheName" $ do
    it "extracts the cache name from a subdomain of the default Cachix host" $
      cachixCacheName defaultHost "https://corestory.cachix.org" `shouldBe` Just "corestory"

    it "ignores a trailing path on the substituter URI" $
      cachixCacheName defaultHost "https://corestory.cachix.org/" `shouldBe` Just "corestory"

    it "returns Nothing for a non-Cachix substituter" $
      cachixCacheName defaultHost "https://cache.nixos.org" `shouldBe` Nothing

    it "returns Nothing for the bare Cachix host (no subdomain)" $
      cachixCacheName defaultHost "https://cachix.org" `shouldBe` Nothing

    it "matches a cache on a self-hosted Cachix instance" $
      cachixCacheName selfHosted "https://foo.cachix.example.com" `shouldBe` Just "foo"

    it "does not match a cache on the default host when using a self-hosted instance" $
      cachixCacheName selfHosted "https://foo.cachix.org" `shouldBe` Nothing

    it "returns Nothing for an unparseable URI" $
      cachixCacheName defaultHost "not a url" `shouldBe` Nothing

defaultHost :: URI
defaultHost = "https://cachix.org"

selfHosted :: URI
selfHosted = "https://cachix.example.com"
