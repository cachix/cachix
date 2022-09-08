{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cachix.API.WebSocketSubprotocol where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM.TMQueue as TMQueue
import qualified Control.Exception.Safe as Exception
import qualified Data.Aeson as Aeson
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Network.WebSockets as WS
import Protolude

data Message cmd = Message
  { method :: Text,
    command :: cmd,
    agent :: Maybe UUID,
    id :: UUID
  }
  deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data Cache = Cache
  { cacheName :: Text,
    publicKey :: Text,
    isPublic :: Bool
  }
  deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data AgentInformation = AgentInformation
  { cache :: Maybe Cache,
    id :: UUID
  }
  deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data DeploymentDetails = DeploymentDetails
  { storePath :: Text,
    id :: UUID,
    index :: Int64,
    rollbackScript :: Maybe Text
  }
  deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data BackendCommand
  = Deployment DeploymentDetails
  | AgentRegistered AgentInformation
  deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data AgentCommand
  = DeploymentStarted {id :: UUID, time :: UTCTime, closureSize :: Maybe Int64}
  | DeploymentFinished {id :: UUID, time :: UTCTime, hasSucceeded :: Bool}
  deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

parseMessage :: Aeson.FromJSON cmd => ByteString -> Either Text (Message cmd)
parseMessage body =
  case Aeson.eitherDecodeStrict' body of
    Left err -> Left $ toS err
    Right message -> Right message

sendMessage :: Aeson.ToJSON cmd => WS.Connection -> Message cmd -> IO ()
sendMessage connection cmd =
  WS.sendTextData connection $ Aeson.encode cmd

-- | Receive and process messages in parallel.
--
-- Note: This will not rethrow the 'CloseRequest' exception!
--
-- TODO: use Async.replicateConcurrently
receiveDataConcurrently :: WS.Connection -> (ByteString -> IO ()) -> IO ()
receiveDataConcurrently connection action =
  do
    queue <- atomically TMQueue.newTMQueue
    Async.withAsync (consumer queue) (producer queue)
  where
    producer queue consumerThread =
      loop
        `Exception.catch` closeRequest
        `Exception.finally` closeGracefully queue consumerThread
      where
        loop = do
          payload <- WS.receiveData connection
          atomically $ TMQueue.writeTMQueue queue payload
          loop

    consumer queue = loop
      where
        loop = do
          payload <- atomically $ TMQueue.readTMQueue queue
          case payload of
            Nothing -> return ()
            Just msg -> action msg *> loop

    -- Close the queue and wait for the consumer finish processing messages.
    closeGracefully :: TMQueue.TMQueue a -> Async.Async () -> IO ()
    closeGracefully queue consumerThread = do
      atomically $ TMQueue.closeTMQueue queue
      Async.wait consumerThread

    closeRequest :: WS.ConnectionException -> IO ()
    closeRequest (WS.CloseRequest _ _) = return ()
    closeRequest e = throwIO e

data Log = Log
  { line :: Text,
    time :: UTCTime
  }
  deriving (Generic, Show, Aeson.ToJSON, Aeson.FromJSON)
