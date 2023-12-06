module Cachix.Client.Daemon.Log where

import Cachix.Client.Daemon.Types.Log
import qualified Control.Monad.Catch as E
import qualified Katip
import Protolude

new :: (MonadIO m) => Katip.Namespace -> Maybe Handle -> LogLevel -> m Logger
new logLabel logHandle logLevel = do
  logKLogEnv <- liftIO $ Katip.initLogEnv logLabel ""
  let logKNamespace = mempty
  let logKContext = mempty
  return $ Logger {..}

withLogger :: (MonadIO m, E.MonadMask m) => Logger -> (Logger -> m a) -> m a
withLogger logger@(Logger {..}) f = do
  let kLogLevel = toKatipLogLevel logLevel
  let kLogHandle = fromMaybe stdout logHandle
  let registerScribe = liftIO $ do
        scribeHandle <- Katip.mkHandleScribe Katip.ColorIfTerminal kLogHandle (Katip.permitItem kLogLevel) Katip.V2
        Katip.registerScribe "stdout" scribeHandle Katip.defaultScribeSettings logKLogEnv

  E.bracket registerScribe (liftIO . Katip.closeScribes) $ \logEnv ->
    f logger {logKLogEnv = logEnv}

getKatipNamespace :: Logger -> Katip.Namespace
getKatipNamespace = logKNamespace

getKatipContext :: Logger -> Katip.LogContexts
getKatipContext = logKContext

getKatipLogEnv :: Logger -> Katip.LogEnv
getKatipLogEnv = logKLogEnv

localLogEnv f logger = logger {logKLogEnv = f (logKLogEnv logger)}

localKatipContext f logger = logger {logKContext = f (logKContext logger)}

localKatipNamespace f logger = logger {logKNamespace = f (logKNamespace logger)}

toKatipLogLevel :: LogLevel -> Katip.Severity
toKatipLogLevel = \case
  Debug -> Katip.DebugS
  Info -> Katip.InfoS
  Warning -> Katip.WarningS
  Error -> Katip.ErrorS
