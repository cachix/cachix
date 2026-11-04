module EnvSpec (spec) where

import Cachix.Client.Env (customManagerSettings)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (newTlsManagerWith)
import Protolude
import StalledServer (withStalledServer)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = parallel $ describe "HTTP manager timeouts" $ do
  it "times out a TLS handshake when the server accepts but never answers" $
    stalledRequest "https" isConnectionTimeout
  it "times out an HTTP response when the server accepts but never answers" $
    stalledRequest "http" isResponseTimeout

stalledRequest :: Text -> (HTTP.HttpException -> Bool) -> IO ()
stalledRequest scheme expected = withStalledServer $ \port _ -> do
  manager <- newTlsManagerWith $ HTTP.managerSetProxy HTTP.noProxy customManagerSettings
  request <- HTTP.parseRequest (toS scheme <> "://127.0.0.1:" <> show port)
  -- Keep the real manager default so removing the production timeout fails
  -- this test, while bounding the regression's own runtime.
  result <- timeout (45 * 1_000_000) (try (HTTP.httpNoBody request manager))
  case result of
    Just (Left err) -> err `shouldSatisfy` expected
    Just (Right _) -> expectationFailure "Expected the stalled request to time out"
    Nothing -> expectationFailure "The HTTP manager did not time out the stalled request"

isConnectionTimeout :: HTTP.HttpException -> Bool
isConnectionTimeout (HTTP.HttpExceptionRequest _ HTTP.ConnectionTimeout) = True
isConnectionTimeout _ = False

isResponseTimeout :: HTTP.HttpException -> Bool
isResponseTimeout (HTTP.HttpExceptionRequest _ HTTP.ResponseTimeout) = True
isResponseTimeout _ = False
