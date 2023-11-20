{-# LANGUAGE TypeFamilies #-}

module Cachix.Client.Daemon.Event where

import Protolude

class HasEvent m where
  type Key m :: Type
  type Event m :: Type

  pushEvent :: Key m -> Event m -> m ()
  pushStarted :: Key m -> m ()
  pushFinished :: Key m -> m ()
  pushStorePathAttempt :: Key m -> FilePath -> m ()
  pushStorePathProgress :: Key m -> FilePath -> Int -> m ()
  pushStorePathDone :: Key m -> FilePath -> m ()
