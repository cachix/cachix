module Cachix.Client.Daemon.Log
  ( new,
    withLogger,
    getKatipNamespace,
    getKatipContext,
    getKatipLogEnv,
    localLogEnv,
    localKatipContext,
    localKatipNamespace,
    toKatipLogLevel,
    Log.Logger (..),
    Log.LogLevel (..),
  )
where

import Cachix.Client.Daemon.Types.Log as Log
import qualified Control.Monad.Catch as E
import Data.Text.Lazy.Builder
import Katip (renderSeverity)
import qualified Katip
import qualified Katip.Format.Time as Katip.Format
import Katip.Scribes.Handle (brackets, colorBySeverity, getKeys)
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
        scribeHandle <- Katip.mkHandleScribeWithFormatter conciseBracketFormat Katip.ColorIfTerminal kLogHandle (Katip.permitItem kLogLevel) Katip.V2
        Katip.registerScribe "stdout" scribeHandle Katip.defaultScribeSettings logKLogEnv

  E.bracket registerScribe (liftIO . Katip.closeScribes) $ \logEnv ->
    f logger {logKLogEnv = logEnv}

getKatipNamespace :: Logger -> Katip.Namespace
getKatipNamespace = logKNamespace

getKatipContext :: Logger -> Katip.LogContexts
getKatipContext = logKContext

getKatipLogEnv :: Logger -> Katip.LogEnv
getKatipLogEnv = logKLogEnv

localLogEnv :: (Katip.LogEnv -> Katip.LogEnv) -> Logger -> Logger
localLogEnv f logger = logger {logKLogEnv = f (logKLogEnv logger)}

localKatipContext :: (Katip.LogContexts -> Katip.LogContexts) -> Logger -> Logger
localKatipContext f logger = logger {logKContext = f (logKContext logger)}

localKatipNamespace :: (Katip.Namespace -> Katip.Namespace) -> Logger -> Logger
localKatipNamespace f logger = logger {logKNamespace = f (logKNamespace logger)}

toKatipLogLevel :: LogLevel -> Katip.Severity
toKatipLogLevel = \case
  Debug -> Katip.DebugS
  Info -> Katip.InfoS
  Warning -> Katip.WarningS
  Error -> Katip.ErrorS

conciseBracketFormat :: (Katip.LogItem a) => Katip.ItemFormatter a
conciseBracketFormat withColor verbosity Katip.Item {..} =
  brackets nowStr
    <> brackets (fromText (renderSeverity' _itemSeverity))
    <> mconcat ks
    <> fromText " "
    <> Katip.unLogStr _itemMessage
  where
    nowStr = fromText (Katip.Format.formatAsLogTime _itemTime)
    ks = map brackets $ getKeys verbosity _itemPayload
    renderSeverity' severity =
      colorBySeverity withColor severity (renderSeverity severity)
