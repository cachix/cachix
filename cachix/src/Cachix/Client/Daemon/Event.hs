{-# LANGUAGE TypeFamilies #-}

module Cachix.Client.Daemon.Event where

import Protolude

class HasEvent m where
  type Key m :: Type
  type Event m :: Type

  pushEvent :: Key m -> Event m -> m ()
  pushStarted :: Key m -> m ()
  pushFinished :: Key m -> m ()
  pushStorePathAttempt :: Key m -> FilePath -> Int64 -> m ()
  pushStorePathProgress :: Key m -> FilePath -> Int64 -> m ()
  pushStorePathDone :: Key m -> FilePath -> m ()
  pushStorePathFailed :: Key m -> FilePath -> Text -> m ()
