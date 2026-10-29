-- | WebSocket clients with explicit HTTP handshake rejections.
module Cachix.API.WebSocket.Client
  ( HandshakeRejected (..),
    withHandshakeErrors,
  )
where

import qualified Network.WebSockets as WS
import Protolude

newtype HandshakeRejected = HandshakeRejected
  { rejectionResponse :: WS.ResponseHead
  }
  deriving (Show)

instance Exception HandshakeRejected

-- | Normalize HTTP handshake rejections thrown by the supplied action.
-- Invalid HTTP 101 upgrade responses retain their original exception.
withHandshakeErrors :: IO a -> IO a
withHandshakeErrors action = action `catch` handleHandshakeError
  where
    handleHandshakeError (WS.RequestRejected _ response) = throwIO $ HandshakeRejected response
    handleHandshakeError (WS.MalformedResponse response _)
      | WS.responseCode response /= 101 = throwIO $ HandshakeRejected response
    handleHandshakeError exception = throwIO exception
