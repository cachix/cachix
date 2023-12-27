module Cachix.Client.Daemon.Types.Log
  ( LogLevel (..),
    Logger (..),
  )
where

import qualified Katip
import Protolude

-- | The log level to use for logging
--
-- TODO: reuse in deploy agent
data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving stock (Eq, Ord, Show)

data Logger = Logger
  { -- | An optional handle to output logs to.
    -- Defaults to stdout.
    logHandle :: Maybe Handle,
    -- | The log level to use for logging
    logLevel :: LogLevel,
    -- | Logger namespace
    logKNamespace :: Katip.Namespace,
    -- | Logger context
    logKContext :: Katip.LogContexts,
    -- | Logger env
    logKLogEnv :: Katip.LogEnv
  }
