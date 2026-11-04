{-# LANGUAGE DataKinds #-}

module ServantSpec (spec) where

import Cachix.Client.Servant (withResponseTimeout)
import Network.HTTP.Client qualified as HTTP
import Network.Socket.ByteString qualified as Socket
import Protolude
import Servant.API (Get, JSON)
import Servant.Client.Streaming
import StalledServer (withServer)
import Test.Hspec

spec :: Spec
spec = describe "scoped response timeout" $ do
  it "allows a slow response only within the overridden action" $
    withDelayedClient $ \env -> do
      result <- runClientM (withResponseTimeout HTTP.responseTimeoutNone getNumber) env
      result `shouldBe` Right 17
      normalResult <- runClientM getNumber env
      normalResult `shouldSatisfy` isResponseTimeout
  it "keeps the default timeout for other actions" $
    withDelayedClient $ \env -> do
      result <- runClientM getNumber env
      result `shouldSatisfy` isResponseTimeout

getNumber :: ClientM Int
getNumber = client (Proxy :: Proxy (Get '[JSON] Int))

withDelayedClient :: (ClientEnv -> IO a) -> IO a
withDelayedClient action = withServer respond $ \port _ -> do
  manager <-
    HTTP.newManager $
      HTTP.managerSetProxy HTTP.noProxy $
        HTTP.defaultManagerSettings
          { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro 100_000
          }
  action $ mkClientEnv manager (BaseUrl Http "127.0.0.1" port "")
  where
    respond connection = do
      void $ Socket.recv connection 4096
      threadDelay 500_000
      Socket.sendAll connection "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 2\r\nConnection: close\r\n\r\n17"

isResponseTimeout :: Either ClientError a -> Bool
isResponseTimeout (Left (ConnectionError exception)) =
  case fromException exception of
    Just (HTTP.HttpExceptionRequest _ HTTP.ResponseTimeout) -> True
    _ -> False
isResponseTimeout _ = False
