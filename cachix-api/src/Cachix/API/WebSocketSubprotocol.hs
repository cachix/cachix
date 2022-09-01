{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

-- TODO: use Async.replicateConcurrently
recieveDataConcurrently :: WS.Connection -> (ByteString -> IO ()) -> IO ()
recieveDataConcurrently connection m =
  do
    channel <- Chan.newChan
    Async.race_ (producer channel) (consumer channel)
    `catch` ( \(e :: WS.ConnectionException) -> do
                case e of
                  WS.CloseRequest _ _ -> return ()
                  WS.ConnectionClosed -> return ()
                  _ -> throwIO e
            )
  where
    producer channel = forever $ do
      payload <- WS.receiveData connection
      Chan.writeChan channel payload
    consumer channel = forever $ do
      payload <- Chan.readChan channel
      m payload

data Log = Log
  { line :: Text,
    time :: UTCTime
  }
  deriving (Generic, Show, Aeson.ToJSON, Aeson.FromJSON)
