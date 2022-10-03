-- Implement ppong on the client side for WS
-- TODO: upstream to https://github.com/jaspervdj/websockets/issues/159
module Cachix.Deploy.WebsocketPong where

import Data.IORef (IORef)
import qualified Data.IORef as IORef
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import qualified Network.WebSockets as WS
import Protolude

type LastPongState = IORef UTCTime

data WebsocketPongTimeout
  = WebsocketPongTimeout
  deriving (Show)

instance Exception WebsocketPongTimeout

newState :: IO LastPongState
newState = do
  now <- getCurrentTime
  IORef.newIORef now

-- everytime we send a ping we check if we also got a pong back
pingHandler :: LastPongState -> ThreadId -> Int -> IO ()
pingHandler pong threadID maxLastPing = do
  last <- secondsSinceLastPong pong
  when (last > maxLastPing) $
    throwTo threadID WebsocketPongTimeout

secondsSinceLastPong :: LastPongState -> IO Int
secondsSinceLastPong pong = do
  last <- IORef.readIORef pong
  now <- getCurrentTime
  return $ ceiling $ nominalDiffTimeToSeconds $ diffUTCTime now last

pongHandler :: LastPongState -> IO ()
pongHandler pong = do
  now <- getCurrentTime
  void $ IORef.atomicWriteIORef pong now

installPongHandler :: LastPongState -> WS.ConnectionOptions -> WS.ConnectionOptions
installPongHandler pong opts = opts {WS.connectionOnPong = pongHandler pong}
