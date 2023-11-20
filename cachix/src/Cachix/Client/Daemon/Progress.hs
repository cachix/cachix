module Cachix.Client.Daemon.Progress
  ( UploadProgress,
    new,
    complete,
    fail,
    tick,
  )
where

import Cachix.Client.Daemon.Types (PushRetryStatus (..))
import Cachix.Client.HumanSize (humanSize)
import Data.String (String)
import qualified Data.Text as T
import Protolude
import qualified System.Console.AsciiProgress as Ascii
import System.Console.Pretty
import System.IO (hIsTerminalDevice)

data UploadProgress
  = ProgressBar
      { path :: String,
        size :: Int64,
        progressBar :: Ascii.ProgressBar
      }
  | FallbackText
      { path :: String,
        size :: Int64
      }

new :: Handle -> String -> Int64 -> PushRetryStatus -> IO UploadProgress
new hdl path size retryStatus = do
  isTerminal <- liftIO $ hIsTerminalDevice hdl
  if isTerminal
    then do
      progressBar <- newProgressBar path size retryStatus
      return $ ProgressBar {..}
    else do
      hPutStr stderr $ uploadStartFallback path size retryStatus
      return $ FallbackText {..}

complete :: UploadProgress -> IO ()
complete ProgressBar {..} = Ascii.complete progressBar
complete _ = pure ()

tick :: UploadProgress -> Int64 -> IO ()
tick ProgressBar {..} deltaBytes = Ascii.tickN progressBar (fromIntegral deltaBytes)
tick _ _ = pure ()

fail :: UploadProgress -> IO ()
fail ProgressBar {..} =
  Ascii.setConsoleRegion (Ascii.pgRegion progressBar) $
    uploadFailed path
fail FallbackText {path} = hPutStr stderr $ uploadFailed path

newProgressBar :: String -> Int64 -> PushRetryStatus -> IO Ascii.ProgressBar
newProgressBar path size retryStatus = do
  let hSize = toS $ humanSize $ fromIntegral size
  let barLength =
        uploadTickBar path hSize retryStatus
          & toS
          & T.replace "[:bar]" ""
          & T.replace ":percent" "  0%"
          & T.length

  liftIO $
    Ascii.newProgressBar
      Ascii.def
        { Ascii.pgTotal = fromIntegral size,
          -- https://github.com/yamadapc/haskell-ascii-progress/issues/24
          Ascii.pgWidth = 20 + barLength,
          Ascii.pgOnCompletion = Just $ uploadComplete path size,
          Ascii.pgFormat = uploadTickBar path hSize retryStatus
        }

retryText :: PushRetryStatus -> String
retryText PushRetryStatus {retryCount} =
  if retryCount == 0
    then ""
    else color Yellow $ "retry #" <> show retryCount <> " "

uploadComplete :: String -> Int64 -> String
uploadComplete path size =
  let hSize = toS $ humanSize $ fromIntegral size
   in color Green "✓ " <> path <> " (" <> hSize <> ")"

uploadFailed :: String -> String
uploadFailed path =
  color Red "✗ " <> path

uploadTickBar :: String -> String -> PushRetryStatus -> String
uploadTickBar path hSize retryStatus =
  color Blue "[:bar] " <> retryText retryStatus <> toS path <> " (:percent of " <> hSize <> ")"

uploadStartFallback :: String -> Int64 -> PushRetryStatus -> String
uploadStartFallback path size retryStatus =
  let hSize = toS $ humanSize $ fromIntegral size
   in retryText retryStatus <> "Pushing " <> path <> " (" <> hSize <> ")\n"
