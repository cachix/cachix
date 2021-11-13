{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cachix.API.WebSocketSubprotocol where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
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
    index :: Int64
  }
  deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data BackendCommand
  = Deployment DeploymentDetails
  | AgentRegistered AgentInformation
  deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

data AgentCommand
  = DeploymentStarted {id :: UUID, time :: UTCTime}
  | DeploymentFinished {id :: UUID, time :: UTCTime, hasSucceeded :: Bool}
  deriving (Show, Eq, Generic, Aeson.FromJSON, Aeson.ToJSON)

parseMessage :: Aeson.FromJSON cmd => ByteString -> Either Text (Message cmd)
parseMessage body =
  case Aeson.eitherDecodeStrict' body of
    Left err -> Left $ toS err
    Right message -> Right message

sendMessage :: Aeson.ToJSON cmd => WS.Connection -> Message cmd -> IO ()
sendMessage connection command =
  WS.sendTextData connection $ Aeson.encode command

-- TODO: use Async.replicateConcurrently
recieveDataConcurrently :: WS.Connection -> (ByteString -> IO ()) -> IO ()
recieveDataConcurrently connection m = do
  channel <- Chan.newChan
  Async.race_ (producer channel) (consumer channel)
  where
    producer channel = forever $ do
      payload <- WS.receiveData connection
      Chan.writeChan channel payload
    consumer channel = forever $ do
      payload <- Chan.readChan channel
      m payload
