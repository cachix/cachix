-- Implement ppong on the client side for WS
-- TODO: upstream to https://github.com/jaspervdj/websockets/issues/159
module Cachix.Deploy.WebsocketPong where

import Data.IORef
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
  newIORef now

-- everytime we send a ping we check if we also got a pong back
pingHandler :: LastPongState -> Int -> IO ()
pingHandler state maxLastPing = do
  last <- secondsSinceLastPong state
  when (last > maxLastPing) $ do
    throwIO WebsocketPongTimeout

secondsSinceLastPong :: LastPongState -> IO Int
secondsSinceLastPong state = do
  now <- getCurrentTime
  last <- readIORef state
  return $ ceiling $ nominalDiffTimeToSeconds $ diffUTCTime now last

pongHandler :: LastPongState -> IO ()
pongHandler state = do
  now <- getCurrentTime
  writeIORef state now

installPongHandler :: LastPongState -> WS.ConnectionOptions -> WS.ConnectionOptions
installPongHandler state opts = opts {WS.connectionOnPong = pongHandler state}
