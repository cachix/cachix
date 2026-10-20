module CommandListSpec (spec) where

import Cachix.Client.Command.List (Entry (..), cachixCacheName, configuredEntries)
import Cachix.Client.NixConf qualified as NixConf
import Cachix.Client.OptionsParser (ListOptions (..))
import Cachix.Client.URI (URI)
import Protolude
import Test.Hspec

spec :: Spec
spec = do
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

  describe "configuredEntries" $ do
    it "lists active caches but excludes trusted-only and commented caches" $ do
      let source = parsedSource "nix.conf" "substituters = https://live.cachix.org # https://disabled.cachix.org\nextra-substituters = https://extra.cachix.org # https://comment.cachix.org\ntrusted-substituters = https://allowed.cachix.org\nextra-trusted-substituters = https://also-allowed.cachix.org\n"
      map entryUri (configuredEntries defaultHost (ListOptions False) [source])
        `shouldBe` ["https://live.cachix.org", "https://extra.cachix.org"]

    it "includes non-Cachix caches only with --all" $ do
      let source = parsedSource "nix.conf" "substituters = https://foo.cachix.org https://cache.nixos.org\n"
      map entryUri (configuredEntries defaultHost (ListOptions False) [source])
        `shouldBe` ["https://foo.cachix.org"]
      map entryUri (configuredEntries defaultHost (ListOptions True) [source])
        `shouldBe` ["https://foo.cachix.org", "https://cache.nixos.org"]

    it "retains distinct S3 endpoints" $ do
      let urls = ["s3://cache?endpoint=minio-a.example.com", "s3://cache?endpoint=minio-b.example.com"]
          source = NixConf.NixConfSource "nix.conf" (NixConf.NixConf [NixConf.Substituters urls])
      map entryUri (configuredEntries defaultHost (ListOptions True) [source]) `shouldBe` urls

    it "deduplicates equivalent hosts and ports while retaining the first source and spelling" $ do
      let first = parsedSource "global.conf" "substituters = https://FOO.Cachix.Org:443\n"
          second = parsedSource "local.conf" "extra-substituters = https://foo.cachix.org\n"
      map (\entry -> (entrySource entry, entryUri entry)) (configuredEntries defaultHost (ListOptions False) [first, second])
        `shouldBe` [("global.conf", "https://FOO.Cachix.Org:443")]

parsedSource :: FilePath -> Text -> NixConf.NixConfSource
parsedSource path contents =
  case NixConf.parse contents of
    Right conf -> NixConf.NixConfSource path conf
    Left err -> panic (show err)

defaultHost :: URI
defaultHost = "https://cachix.org"

selfHosted :: URI
selfHosted = "https://cachix.example.com"
