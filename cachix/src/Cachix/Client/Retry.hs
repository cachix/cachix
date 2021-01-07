module Cachix.Client.Retry
  ( retryAll,
  )
where

import Control.Exception.Safe (MonadMask)
import Control.Retry (RetryPolicy, RetryStatus, exponentialBackoff, limitRetries, recoverAll)
import Protolude

-- Catches all exceptions except skipAsyncExceptions
retryAll :: (MonadIO m, MonadMask m) => (RetryStatus -> m a) -> m a
retryAll = recoverAll defaultRetryPolicy
  where
    defaultRetryPolicy :: RetryPolicy
    defaultRetryPolicy =
      exponentialBackoff (1000 * 1000) <> limitRetries 5
