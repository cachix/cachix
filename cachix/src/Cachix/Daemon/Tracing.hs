{-# LANGUAGE OverloadedStrings #-}

module Cachix.Daemon.Tracing
  ( withStandaloneTracing,
    withDaemonSpan,
    getDaemonTracer,
    HasTracer (..),
  )
where

import Control.Exception (bracket)
import Data.Text (pack)
import Data.Version (showVersion)
import OpenTelemetry.Attributes (emptyAttributes)
import OpenTelemetry.Trace (InstrumentationLibrary (..), Span, SpanArguments (..), SpanKind (..), Tracer, defaultSpanArguments, getGlobalTracerProvider, inSpan', initializeGlobalTracerProvider, makeTracer, shutdownTracerProvider, tracerOptions)
import Paths_cachix qualified as Paths
import Protolude hiding (bracket)
import UnliftIO (MonadUnliftIO)

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
