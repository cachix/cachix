{-# LANGUAGE OverloadedStrings #-}

module Cachix.Daemon.Tracing
  ( withStandaloneTracing,
    withDaemonSpan,
    getDaemonTracer,
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

withDaemonSpan :: (MonadUnliftIO m) => Text -> SpanKind -> (Span -> m a) -> m a
withDaemonSpan name kind f = do
  tracer <- getDaemonTracer
  let args =
        defaultSpanArguments
          { kind = kind
          }
  inSpan' tracer name args f
