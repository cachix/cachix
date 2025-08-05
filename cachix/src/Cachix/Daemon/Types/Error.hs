module Cachix.Daemon.Types.Error
  ( DaemonError (..),
    UnhandledException (..),
    HasExitCode (..),
  )
where

import Cachix.Daemon.Types.EventLoop (EventLoopError (..))
import Protolude

-- | An error that can occur in the daemon.
--
-- These should not escape the main 'run' function.
data DaemonError
  = -- | The daemon shut down due to an unhandled exception
    DaemonUnhandledException UnhandledException
  | -- | There was an error in the daemon's event loop
    DaemonEventLoopError EventLoopError
  | -- | There was an error with the daemon's socket
    DaemonSocketError
  | -- | Failed to push some store paths
    DaemonPushFailure
  | -- | A command is not supported by the daemon configuration
    DaemonUnsupportedCommand Text
  deriving stock (Show, Eq)

instance Exception DaemonError where
  displayException =
    toS . \case
      DaemonUnhandledException e ->
        unlines ["Unhandled exception in the Cachix Daemon:", "", show e]
      DaemonEventLoopError eventLoopError ->
        unlines ["The daemon encountered an internal event loop error:", "", toS (displayException eventLoopError)]
      DaemonSocketError -> "There was an error with the socket"
      DaemonPushFailure -> "Failed to push some store paths"
      DaemonUnsupportedCommand msg -> toS msg

-- | A wrapper for SomeException that implements equality.
newtype UnhandledException
  = UnhandledException SomeException
  deriving stock (Show)

instance Eq UnhandledException where
  UnhandledException e1 == UnhandledException e2 =
    displayException e1 == displayException e2

-- Convert to an error to an integer exit code

class HasExitCode a where
  toExitCode :: a -> ExitCode

  toExitCodeInt :: a -> Int
  toExitCodeInt x = case toExitCode x of
    ExitSuccess -> 0
    ExitFailure n -> n

instance HasExitCode DaemonError where
  toExitCode err = ExitFailure $ case err of
    DaemonPushFailure -> 3
    DaemonUnsupportedCommand _ -> 2
    _remainingErrors -> 1

instance (HasExitCode a) => HasExitCode (Maybe a) where
  toExitCode Nothing = ExitSuccess
  toExitCode (Just e) = toExitCode e

instance (HasExitCode e) => HasExitCode (Either e a) where
  toExitCode (Right _) = ExitSuccess
  toExitCode (Left e) = toExitCode e
