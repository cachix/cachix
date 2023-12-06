{-# LANGUAGE TypeFamilies #-}

module Cachix.Client.Daemon.Event where

import Control.Retry (RetryStatus)
import Protolude

class HasEvent m where
  type Key m :: Type
  type Event m :: Type

  pushEvent :: Key m -> Event m -> m ()
  pushStarted :: Key m -> m ()
  pushFinished :: Key m -> m ()
  pushStorePathAttempt :: FilePath -> Int64 -> RetryStatus -> m ()
  pushStorePathProgress :: FilePath -> Int64 -> Int64 -> m ()
  pushStorePathDone :: FilePath -> m ()
  pushStorePathFailed :: FilePath -> Text -> m ()
