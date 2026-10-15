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

    it "matches case-insensitively on the substituter host" $
      cachixCacheName defaultHost "https://FOO.Cachix.Org" `shouldBe` Just "foo"

    it "matches case-insensitively on the Cachix host" $
      cachixCacheName "https://Cachix.Org" "https://foo.cachix.org" `shouldBe` Just "foo"

    it "returns Nothing for a URI with no authority (urn:…) instead of throwing" $
      cachixCacheName defaultHost "urn:isbn:0451450523" `shouldBe` Nothing

    it "returns Nothing for a mailto: URI instead of throwing" $
      cachixCacheName defaultHost "mailto:ops@example.com" `shouldBe` Nothing

    it "ignores an explicit default port on the substituter URI" $
      cachixCacheName defaultHost "https://foo.cachix.org:443" `shouldBe` Just "foo"

defaultHost :: URI
defaultHost = "https://cachix.org"

selfHosted :: URI
selfHosted = "https://cachix.example.com"
