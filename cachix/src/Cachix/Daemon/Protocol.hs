{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cachix.Daemon.Protocol
  ( ClientMessage (..),
    DaemonMessage (..),
    DaemonExitStatus (..),
    PushRequestId,
    newPushRequestId,
    PushRequest (..),
    newMessage,
    splitMessages,
  )
where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Protolude

-- | JSON messages that the client can send to the daemon
data ClientMessage
  = ClientPushRequest !PushRequest
  | ClientStop
  | ClientPing
  | ClientSubscribed
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | JSON messages that the daemon can send to the client
data DaemonMessage
  = DaemonPong
  | DaemonExit !DaemonExitStatus
  | PushStarted
  | PushCompleted
  | PushFailed Text
  | PushProgress FilePath Int
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

data DaemonExitStatus = DaemonExitStatus
  { exitCode :: !Int,
    exitMessage :: !(Maybe Text)
  }
  deriving stock (Eq, Generic, Show)
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
