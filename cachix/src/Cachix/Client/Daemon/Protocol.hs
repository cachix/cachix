{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cachix.Client.Daemon.Protocol
  ( ClientMessage (..),
    DaemonMessage (..),
    PushRequestId,
    newPushRequestId,
    PushRequest (..),
    newMessage,
    splitMessages,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Protolude

-- | JSON messages that the client can send to the daemon
data ClientMessage
  = ClientPushRequest !PushRequest
  | ClientStop
  | ClientPing
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | JSON messages that the daemon can send to the client
data DaemonMessage
  = DaemonPong
  | DaemonBye
  deriving stock (Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

newtype PushRequestId = PushRequestId UUID
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Aeson.FromJSON, Aeson.ToJSON, Hashable)

newPushRequestId :: (MonadIO m) => m PushRequestId
newPushRequestId = liftIO $ PushRequestId <$> UUID.nextRandom

-- | A request for the daemon to push store paths to a binary cache
data PushRequest = PushRequest
  { storePaths :: [FilePath]
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

newMessage :: (Aeson.ToJSON a) => a -> LBS.ByteString
newMessage msg =
  Aeson.encode msg `LBS.snoc` fromIntegral (fromEnum '\n')

splitMessages :: ByteString -> ([ByteString], ByteString)
splitMessages = go []
  where
    go acc bs =
      case BS.break (== _n) bs of
        (line, "") -> (reverse acc, line)
        (line, "\n") -> go (line `cons` acc) mempty
        (line, rest) ->
          go (line `cons` acc) (BS.drop 1 rest)

    cons "" xs = xs
    cons x xs = x : xs

    _n = fromIntegral (fromEnum '\n')
