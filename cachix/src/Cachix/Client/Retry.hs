module Cachix.Client.Retry
  ( retryAll,
    retryAllWithLogging,
    endlessRetryPolicy,
  )
where

import Control.Exception.Safe (MonadMask, isSyncException)
import Control.Retry (RetryPolicy, RetryPolicyM, RetryStatus, exponentialBackoff, limitRetries, logRetries, recoverAll, recovering, skipAsyncExceptions)
import Protolude

retryAll :: (MonadIO m, MonadMask m) => (RetryStatus -> m a) -> m a
retryAll =
  recoverAll defaultRetryPolicy

-- Catches all exceptions except async exceptions with logging support
retryAllWithLogging :: (MonadIO m, MonadMask m) => RetryPolicyM m -> (Bool -> SomeException -> RetryStatus -> m ()) -> m a -> m a
retryAllWithLogging retryPolicy logger action = recovering retryPolicy handlers $ const action
  where
    handlers = skipAsyncExceptions ++ [loggingHandler]

    loggingHandler = logRetries exceptionPredicate logger

    exceptionPredicate = return . isSyncException

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy =
  exponentialBackoff (1000 * 1000) <> limitRetries 5

endlessRetryPolicy :: RetryPolicy
endlessRetryPolicy =
  exponentialBackoff (1000 * 1000)
