module URISpec where

import Cachix.Client.URI qualified as URI
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Either.Extra
import Dhall qualified
import Dhall.Core qualified
import Protolude
import Servant.Client.Core qualified as Servant
import Test.Hspec

secureScheme :: ByteString
secureScheme = "https"

unsecureScheme :: ByteString
unsecureScheme = "http"

host :: ByteString
host = "cachix.org"

subdomain :: Text
subdomain = "foo"

secureUri :: ByteString
secureUri = secureScheme <> "://" <> host

unsecureUri :: ByteString
unsecureUri = unsecureScheme <> "://" <> host

secureUriWithSubdomain :: ByteString
secureUriWithSubdomain = secureScheme <> "://" <> encodeUtf8 subdomain <> "." <> host

-- A helper that throws an error when parsing fails.
parseURI' :: ByteString -> URI.URI
parseURI' = fromRight' . URI.parseURI

spec :: Spec
spec =
  describe "URI" $ do
    describe "canonicalForm" $ do
      it "normalizes scheme, hostname and default port" $
        URI.canonicalForm "HTTPS://FOO.Cachix.Org:443" `shouldBe` Just "https://foo.cachix.org"

      it "preserves credentials, path, query parameters and fragment" $
        URI.canonicalForm "https://user:pass@FOO.cachix.org:443/cache/?priority=10#fragment"
          `shouldBe` Just "https://user:pass@foo.cachix.org/cache/?priority=10#fragment"

      it "preserves non-default ports" $
        URI.canonicalForm "https://foo.cachix.org:8443" `shouldBe` Just "https://foo.cachix.org:8443"

      it "preserves the trailing slash on a nonempty path" $
        URI.canonicalForm "https://foo.cachix.org/cache/" `shouldNotBe` URI.canonicalForm "https://foo.cachix.org/cache"

      it "returns Nothing without an authority" $
        URI.canonicalForm "urn:isbn:0451450523" `shouldBe` Nothing

    it "parses a URI" $
      URI.parseURI secureUri `shouldSatisfy` isRight

    it "re-serializes a URI" $
      URI.serialize <$> URI.parseURI secureUri `shouldBe` Right secureUri

    it "appends a subdomain" $ do
      let parsedURI = parseURI' secureUri
          newURI = URI.appendSubdomain subdomain parsedURI
      URI.serialize newURI `shouldBe` secureUriWithSubdomain

    it "returns the hostname" $
      URI.getHostname (parseURI' secureUri) `shouldBe` URI.Host host

    it "returns port 80 for HTTP URIs" $ do
      let scheme = URI.getScheme (parseURI' unsecureUri)
      URI.getPortFor scheme `shouldBe` Just (URI.Port 80)

    it "returns port 443 for HTTPS URIs" $ do
      let scheme = URI.getScheme (parseURI' secureUri)
      URI.getPortFor scheme `shouldBe` Just (URI.Port 443)

    it "detects if SSL is required" $ do
      let getScheme = URI.getScheme . parseURI'
      URI.requiresSSL (getScheme secureUri) `shouldBe` True
      URI.requiresSSL (getScheme unsecureUri) `shouldBe` False

    it "converts to JSON" $
      Aeson.encode (parseURI' secureUri) `shouldBe` "\"" <> BL.fromStrict secureUri <> "\""

    it "converts from JSON" $
      Aeson.decodeStrict' ("\"" <> secureUri <> "\"") `shouldBe` Just (parseURI' secureUri)

    it "converts to Dhall" $ do
      let asDhall = Dhall.embed Dhall.inject (parseURI' secureUri)
      Dhall.Core.pretty asDhall `shouldBe` "\"" <> decodeUtf8 secureUri <> "\""

    it "converts from Dhall" $
      Dhall.input Dhall.auto ("\"" <> decodeUtf8 secureUri <> "\"") `shouldReturn` parseURI' secureUri

    -- https://github.com/cachix/cachix/issues/462
    it "converts to a Servant BaseUrl without trailing slashes" $ do
      let uriWithTrailingSlash = secureUri <> "/"
          uri = parseURI' uriWithTrailingSlash
      Servant.baseUrlPath (URI.getBaseUrl uri) `shouldBe` ""
