module CommandPinSpec (spec) where

import Cachix.Client.OptionsParser
import Cachix.Types.PinCreate (Keep (..))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Network.Socket.ByteString qualified as Socket
import Protolude
import StalledServer (withServer)
import System.Environment (getEnvironment, withArgs)
import System.IO.Temp (withSystemTempDirectory)
import System.Process qualified as Process
import Test.Hspec

spec :: Spec
spec = do
  describe "pin command parsing" $ do
    it "preserves pin creation with artifacts and retention" $ do
      (_, command) <- withArgs ["pin", "mycache", "release", "/nix/store/example", "--artifact", "bin/tool", "--keep-days", "3"] getOpts
      case command of
        Pin opts -> do
          pinCacheName opts `shouldBe` "mycache"
          pinName opts `shouldBe` "release"
          pinStorePath opts `shouldBe` "/nix/store/example"
          pinArtifacts opts `shouldBe` ["bin/tool"]
          pinKeep opts `shouldBe` Just (Days 3)
        other -> expectationFailure $ "Expected pin creation, got " <> show other

    for_ ["get", "create"] $ \cacheName ->
      it ("preserves pin creation for a cache named " <> cacheName) $ do
        (_, command) <- withArgs ["pin", cacheName, "release", "/nix/store/example"] getOpts
        case command of
          Pin opts -> do
            pinCacheName opts `shouldBe` toS cacheName
            pinName opts `shouldBe` "release"
            pinStorePath opts `shouldBe` "/nix/store/example"
          other -> expectationFailure $ "Expected pin creation, got " <> show other

    it "parses a literal pin name containing a colon" $ do
      (_, command) <- withArgs ["pins", "get", "mycache", "mypin:1.0.0", "--json"] getOpts
      case command of
        PinGet opts -> do
          pinGetCacheName opts `shouldBe` "mycache"
          pinGetName opts `shouldBe` "mypin:1.0.0"
          pinGetJson opts `shouldBe` True
        other -> expectationFailure $ "Expected pin lookup, got " <> show other

  describe "pins get" $ do
    it "outputs a JSON object without requiring authentication for a public cache" $ do
      (code, output, _) <- runLookup "200 OK" pinList ["--json"]
      code `shouldBe` ExitSuccess
      (Aeson.eitherDecodeStrict $ encodeUtf8 (toS output :: Text) :: Either [Char] Aeson.Value) `shouldBe` Aeson.eitherDecodeStrict metadata

    it "shows metadata in readable output by default" $ do
      (code, output, _) <- runLookup "200 OK" pinList []
      code `shouldBe` ExitSuccess
      output `shouldContain` "Name: mypin:1.0.0"
      output `shouldContain` "Revision: 2"
      output `shouldContain` "Retention: Revisions from the last 3 days"
      output `shouldContain` "Artifact: bin/tool"

    it "exits unsuccessfully for a missing pin without JSON on stdout" $ do
      (code, output, errors) <- runLookup "200 OK" ("[" <> metadataFor "mypin:1.0.0-other" <> "]") ["--json"]
      code `shouldSatisfy` (/= ExitSuccess)
      output `shouldBe` ""
      errors `shouldContain` "Pin mypin:1.0.0 was not found in cache mycache."

    it "reports a missing pin when the cache has no pins" $ do
      (code, output, errors) <- runLookup "200 OK" "[]" ["--json"]
      code `shouldSatisfy` (/= ExitSuccess)
      output `shouldBe` ""
      errors `shouldContain` "Pin mypin:1.0.0 was not found in cache mycache."

    it "preserves authorization failures from the listing endpoint" $ do
      (code, output, errors) <- runLookup "403 Forbidden" "" ["--json"]
      code `shouldSatisfy` (/= ExitSuccess)
      output `shouldBe` ""
      errors `shouldContain` "403"
      errors `shouldNotContain` "was not found in cache"

pinList :: ByteString
pinList = "[" <> metadataFor "mypin:1.0.0-other" <> "," <> metadata <> "]"

metadata :: ByteString
metadata = metadataFor "mypin:1.0.0"

metadataFor :: ByteString -> ByteString
metadataFor name = "{\"name\":\"" <> name <> "\",\"createdOn\":\"2026-01-01T00:00:00Z\",\"keep\":{\"tag\":\"Days\",\"contents\":3},\"lastRevision\":{\"storePath\":\"/nix/store/example\",\"revision\":2,\"createdOn\":\"2026-01-02T00:00:00Z\",\"artifacts\":[\"bin/tool\"],\"orphaned\":false}}"

runLookup :: ByteString -> ByteString -> [Text] -> IO (ExitCode, [Char], [Char])
runLookup status body extra =
  withSystemTempDirectory "cachix-pin-get" $ \dir ->
    withServer respond $ \port _ -> do
      environment <- getEnvironment
      let overrides = [("XDG_CONFIG_HOME", dir), ("no_proxy", "127.0.0.1"), ("NO_PROXY", "127.0.0.1")]
          cleanEnvironment = filter (\(key, _) -> key `notElem` (["CACHIX_AUTH_TOKEN", "CACHIX_SIGNING_KEY"] <> map fst overrides)) environment
          args = ["--config", dir <> "/cachix.dhall", "--hostname", "http://127.0.0.1:" <> show port, "pins", "get", "mycache", "mypin:1.0.0"] <> map toS extra
      Process.readCreateProcessWithExitCode (Process.proc "cachix" args) {Process.env = Just $ overrides <> cleanEnvironment} ""
  where
    respond connection = do
      headers <- readHeaders connection ""
      let correctEndpoint = "GET /api/v1/cache/mycache/pin " `BS.isPrefixOf` headers
          responseStatus = if correctEndpoint then status else "404 Not Found"
          responseBody = if correctEndpoint then body else ""
      Socket.sendAll connection $ "HTTP/1.1 " <> responseStatus <> "\r\nContent-Type: application/json\r\nConnection: close\r\nContent-Length: " <> show (BS.length responseBody) <> "\r\n\r\n" <> responseBody
    readHeaders connection received
      | "\r\n\r\n" `BS.isInfixOf` received = pure received
      | otherwise = do
          chunk <- Socket.recv connection 4096
          if BS.null chunk then pure received else readHeaders connection (received <> chunk)
