module URISpec where

import qualified Cachix.Client.URI as URI
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as BL
import Data.Either.Extra
import qualified Dhall
import qualified Dhall.Core
import Protolude
import qualified Servant.Client.Core as Servant
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
    it "parses a URI" $
      URI.parseURI secureUri `shouldSatisfy` isRight

    it "re-serializes a URI" $
      URI.serialize <$> URI.parseURI secureUri `shouldBe` Right secureUri

    it "appends a subdomain" $
      let parsedURI = parseURI' secureUri
          newURI = URI.appendSubdomain subdomain parsedURI
       in URI.serialize newURI `shouldBe` secureUriWithSubdomain

    it "returns the hostname" $
      URI.getHostname (parseURI' secureUri) `shouldBe` URI.Host host

    it "returns port 80 for HTTP URIs" $
      let scheme = URI.getScheme (parseURI' unsecureUri)
       in URI.getPortFor scheme `shouldBe` Just (URI.Port 80)

    it "returns port 443 for HTTPS URIs" $
      let scheme = URI.getScheme (parseURI' secureUri)
       in URI.getPortFor scheme `shouldBe` Just (URI.Port 443)

    it "detects if SSL is required" $
      let getScheme = URI.getScheme . parseURI'
       in do
            URI.requiresSSL (getScheme secureUri) `shouldBe` True
            URI.requiresSSL (getScheme unsecureUri) `shouldBe` False

    it "converts to JSON" $
      Aeson.encode (parseURI' secureUri) `shouldBe` "\"" <> BL.fromStrict secureUri <> "\""

    it "converts from JSON" $
      Aeson.decodeStrict' ("\"" <> secureUri <> "\"") `shouldBe` Just (parseURI' secureUri)

    it "converts to Dhall" $
      let asDhall = Dhall.embed Dhall.inject (parseURI' secureUri)
       in Dhall.Core.pretty asDhall `shouldBe` "\"" <> decodeUtf8 secureUri <> "\""

    it "converts from Dhall" $
      Dhall.input Dhall.auto ("\"" <> decodeUtf8 secureUri <> "\"") `shouldReturn` parseURI' secureUri

    -- https://github.com/cachix/cachix/issues/462
    it "converts to a Servant BaseUrl without trailing slashes" $
      let uriWithTrailingSlash = secureUri <> "/"
          uri = parseURI' uriWithTrailingSlash
       in Servant.baseUrlPath (URI.getBaseUrl uri) `shouldBe` ""
