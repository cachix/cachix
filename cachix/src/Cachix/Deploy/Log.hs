{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Cachix.Deploy.Log where

import qualified Cachix.API.WebSocketSubprotocol as WSS
import qualified Control.Concurrent.STM.TMQueue as TMQueue
import Control.Exception.Safe (MonadMask, bracket)
import qualified Data.Aeson as Aeson
import Data.Conduit ((.|))
import qualified Data.Conduit as Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.TQueue as Conduit
import Data.Time.Clock (getCurrentTime)
import qualified Katip
import qualified Network.WebSockets as WS
import Protolude hiding (bracket, toS)
import Protolude.Conv (toS)

-- A temporary crutch while we fix up the types
type WithLog = Katip.KatipContextT IO () -> IO ()

data Options = Options
  { -- | The minimum verbosity level
    verbosity :: Verbosity,
    -- | The logging namespace, e.g. agent
    namespace :: Katip.Namespace,
    -- | The logging environment, e.g. production
    environment :: Katip.Environment
  }
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

data Verbosity
  = Normal
  | Verbose
  deriving stock (Show, Generic)
  deriving anyclass (Aeson.ToJSON, Aeson.FromJSON)

withLog ::
  (MonadIO m, MonadMask m) =>
  Options ->
  (WithLog -> m a) ->
  m a
withLog Options {..} inner =
  let permit = case verbosity of
        Verbose -> Katip.DebugS
        Normal -> Katip.InfoS

      createLogEnv = liftIO $ do
        logEnv <- Katip.initLogEnv namespace environment
        stdoutScribe <- Katip.mkHandleScribe Katip.ColorIfTerminal stdout (Katip.permitItem permit) Katip.V2
        Katip.registerScribe "stdout" stdoutScribe Katip.defaultScribeSettings logEnv

      createContext logEnv = Katip.runKatipContextT logEnv () namespace

      closeLog = liftIO . Katip.closeScribes
   in bracket createLogEnv closeLog $ inner . createContext

-- Streaming log

type LogStream = Conduit.ConduitT ByteString Conduit.Void IO ()

-- TODO: prepend katip-like format to each line
streamLog :: WithLog -> WS.Connection -> TMQueue.TMQueue ByteString -> IO ()
streamLog logger connection queue = do
  Conduit.runConduit $
    Conduit.sourceTMQueue queue
      .| Conduit.linesUnboundedAscii
      .| logLocal logger
      .| sendLog connection

streamLine :: LogStream -> ByteString -> IO ()
streamLine logStream msg = Conduit.connect (Conduit.yield $ "\n" <> msg <> "\n") logStream

logLocal :: WithLog -> Conduit.ConduitT ByteString ByteString IO ()
logLocal logger =
  Conduit.mapM $ \bs -> do
    logger . Katip.logLocM Katip.DebugS . Katip.ls $ bs
    return bs

sendLog :: WS.Connection -> LogStream
sendLog connection = Conduit.mapM_ $ \bs -> do
  now <- getCurrentTime
  WS.sendTextData connection $ Aeson.encode $ WSS.Log {WSS.line = toS bs, WSS.time = now}
