module WebSocketClientSpec where

import Cachix.API.WebSocket.Client
import qualified Data.IORef as IORef
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as Stream
import Protolude
import Test.Hspec

spec :: Spec
spec = describe "WebSocket handshake errors" $ do
  for_ [(400, "Bad Request"), (401, "Unauthorized")] $ \(status, message) ->
    it ("exposes HTTP " <> show status <> " rejections with their response headers") $ do
      let response = WS.ResponseHead status message [("WWW-Authenticate", "Bearer")]
          rawResponse = "HTTP/1.1 " <> encodeUtf8 (show status :: Text) <> " " <> message <> "\r\nWWW-Authenticate: Bearer\r\n\r\n"
      handshake rawResponse `shouldThrow` rejectsWith response

  it "preserves malformed HTTP 101 upgrade errors" $
    handshake "HTTP/1.1 101 Switching Protocols\r\nSec-WebSocket-Accept: invalid\r\n\r\n"
      `shouldThrow` \case
        WS.MalformedResponse actual _ -> WS.responseCode actual == 101
        _ -> False

  it "preserves failures without an HTTP response" $
    handshake ""
      `shouldThrow` \case
        WS.ParseException _ -> True
        _ -> False

rejectsWith :: WS.ResponseHead -> HandshakeRejected -> Bool
rejectsWith expected (HandshakeRejected actual) =
  WS.responseCode actual == WS.responseCode expected
    && WS.responseMessage actual == WS.responseMessage expected
    && WS.responseHeaders actual == WS.responseHeaders expected

-- Exercise the real client handshake parser without opening a socket.
handshake :: ByteString -> IO ()
handshake response = do
  input <- IORef.newIORef (Just response)
  bracket
    (Stream.makeStream (IORef.atomicModifyIORef' input (\chunk -> (Nothing, chunk))) (const $ pure ()))
    Stream.close
    (\stream -> withHandshakeErrors $ WS.runClientWithStream stream "localhost" "/ws" WS.defaultConnectionOptions [] (const $ pure ()))
