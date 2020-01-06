module Cachix.Api.Error
  ( escalate,
    escalateAs,
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (throwM))
import Protolude

-- | Examples:
--    > escalate . maybeToEither err404
--
--  | Note that exceptions gets handled by warp and logged (user sees just "Internal server")
escalateAs ::
  (Exception exc, MonadThrow m) =>
  (l -> exc) ->
  Either l a ->
  m a
escalateAs f = either (throwM . f) pure

escalate ::
  (Exception exc, MonadThrow m) =>
  Either exc a ->
  m a
escalate = escalateAs identity
