{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Cachix.Deploy.Log where

import qualified Data.Aeson as Aeson
import qualified Katip
import Protolude hiding (bracket)
import UnliftIO (MonadUnliftIO, bracket)

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
  MonadUnliftIO m =>
  Options ->
  (Katip.LogEnv -> m a) ->
  m a
withLog Options {..} =
  let permit = case verbosity of
        Verbose -> Katip.DebugS
        Normal -> Katip.InfoS

      createLogEnv = liftIO $ do
        logEnv <- Katip.initLogEnv namespace environment
        stdoutScribe <- Katip.mkHandleScribe Katip.ColorIfTerminal stdout (Katip.permitItem permit) Katip.V2
        Katip.registerScribe "stdout" stdoutScribe Katip.defaultScribeSettings logEnv

      closeLog = liftIO . Katip.closeScribes
   in bracket createLogEnv closeLog
