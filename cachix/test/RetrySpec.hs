module RetrySpec where

import Cachix.Client.Retry (shouldRetryHttpStatusCode)
import Network.HTTP.Types.Status qualified as HTTP
import Protolude
import Test.Hspec

spec :: Spec
spec = describe "HTTP retry status selection" $
  it "retries timeouts, rate limits and server errors, but not other client errors" $ do
    forM_ [408, 429, 500, 502, 503, 504, 520, 524] $ \code ->
      shouldRetryHttpStatusCode (HTTP.mkStatus code "") `shouldBe` True
    forM_ [400, 401, 403, 404, 409, 413, 421, 425] $ \code ->
      shouldRetryHttpStatusCode (HTTP.mkStatus code "") `shouldBe` False
