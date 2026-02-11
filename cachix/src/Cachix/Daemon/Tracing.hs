{-# LANGUAGE OverloadedStrings #-}

module Cachix.Daemon.Tracing
  ( withStandaloneTracing,
    withDaemonSpan,
    createDaemonSpan,
    endDaemonSpan,
    withChildSpanOf,
    getDaemonTracer,
    HasTracer (..),
  )
where

import Control.Exception (bracket)
import Data.Text (pack)
import Data.Version (showVersion)
import OpenTelemetry.Attributes (emptyAttributes)
import OpenTelemetry.Context (insertSpan)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal (attachContext)
import OpenTelemetry.Trace (InstrumentationLibrary (..), Span, SpanArguments (..), SpanKind (..), Tracer, createSpanWithoutCallStack, defaultSpanArguments, endSpan, getGlobalTracerProvider, inSpan', initializeGlobalTracerProvider, makeTracer, shutdownTracerProvider, tracerOptions)
import Paths_cachix qualified as Paths
import Protolude hiding (bracket, finally)
import UnliftIO (MonadUnliftIO, finally)

class HasTracer env where
  getTracer :: env -> Tracer

daemonInstrumentationLibrary :: InstrumentationLibrary
daemonInstrumentationLibrary =
  InstrumentationLibrary
    { libraryName = "cachix.daemon",
      libraryVersion = pack (showVersion Paths.version),
      librarySchemaUrl = "",
      libraryAttributes = emptyAttributes
    }

withStandaloneTracing :: IO a -> IO a
withStandaloneTracing action =
  bracket initializeGlobalTracerProvider shutdownTracerProvider (const action)

getDaemonTracer :: (MonadIO m) => m Tracer
getDaemonTracer = do
  provider <- getGlobalTracerProvider
  pure $ makeTracer provider daemonInstrumentationLibrary tracerOptions

withDaemonSpan :: (MonadUnliftIO m, MonadReader env m, HasTracer env) => Text -> SpanKind -> (Span -> m a) -> m a
withDaemonSpan name kind f = do
  tracer <- asks getTracer
  inSpan' tracer name (defaultSpanArguments {kind = kind}) f

-- | Long-lived span with explicit parent context.
-- Use Context.empty for a root span. Caller must call endDaemonSpan.
createDaemonSpan :: (MonadIO m, MonadReader env m, HasTracer env) => Context.Context -> Text -> SpanKind -> m Span
createDaemonSpan parentCtx name spanKind = do
  tracer <- asks getTracer
  liftIO $ createSpanWithoutCallStack tracer parentCtx name (defaultSpanArguments {kind = spanKind})

endDaemonSpan :: (MonadIO m) => Span -> m ()
endDaemonSpan s = liftIO $ endSpan s Nothing

-- | Run action as a child of parentSpan.
-- Sets thread-local context so nested withDaemonSpan calls also parent correctly.
withChildSpanOf :: (MonadUnliftIO m, MonadReader env m, HasTracer env) => Span -> Text -> SpanKind -> (Span -> m a) -> m a
withChildSpanOf parentSpan name spanKind f = do
  tracer <- asks getTracer
  let parentCtx = insertSpan parentSpan Context.empty
  mOldCtx <- liftIO $ attachContext parentCtx
  inSpan' tracer name (defaultSpanArguments {kind = spanKind}) f
    `finally` liftIO (for_ mOldCtx (void . attachContext))
