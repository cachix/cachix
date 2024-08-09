module Cachix.Daemon.Progress
  ( UploadProgress,
    new,
    complete,
    fail,
    tick,
    update,
  )
where

import Cachix.Client.HumanSize (humanSize)
import Cachix.Daemon.Types (PushRetryStatus (..))
import Control.Concurrent.Async qualified as Async
import Control.Concurrent.MVar
import Data.String (String)
import Data.Text qualified as T
import Protolude
import System.Console.AsciiProgress qualified as Ascii
import System.Console.AsciiProgress.Internal qualified as Ascii.Internal
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
  clearAsciiWith progressBar (uploadFailed path)
fail FallbackText {path} =
  hPutStr stderr $ uploadFailed path

update :: UploadProgress -> PushRetryStatus -> IO UploadProgress
update pg@ProgressBar {..} retryStatus = do
  let opts = newProgressBarOptions path size retryStatus
  pg' <- newAsciiProgressBarInPlace progressBar opts

  return $ pg {progressBar = pg'}
update fbt _ = return fbt

newProgressBar :: String -> Int64 -> PushRetryStatus -> IO Ascii.ProgressBar
newProgressBar path size retryStatus =
  newAsciiProgressBar $ newProgressBarOptions path size retryStatus

newProgressBarOptions :: String -> Int64 -> PushRetryStatus -> Ascii.Options
newProgressBarOptions path size retryStatus = do
  let hSize = toS $ humanSize $ fromIntegral size
  let barLength =
        uploadTickBar path hSize retryStatus
          & toS
          & T.replace "[:bar]" ""
          & T.replace ":percent" "  0%"
          & T.length
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

-- Internal

newAsciiProgressBar :: Ascii.Options -> IO Ascii.ProgressBar
newAsciiProgressBar opts = do
  region <- Ascii.openConsoleRegion Ascii.Linear
  newAsciiProgressBarWithRegion opts region

-- | Create a new progress bar in place of an existing one, reusing the console region.
newAsciiProgressBarInPlace :: Ascii.ProgressBar -> Ascii.Options -> IO Ascii.ProgressBar
newAsciiProgressBarInPlace pg opts = do
  cancelUpdates pg
  newAsciiProgressBarWithRegion opts (Ascii.pgRegion pg)

-- | Create a new progress bar using the provided console region.
newAsciiProgressBarWithRegion :: Ascii.Options -> Ascii.ConsoleRegion -> IO Ascii.ProgressBar
newAsciiProgressBarWithRegion opts region = do
  info <- Ascii.Internal.newProgressBarInfo opts

  -- Display initial progress-bar
  pgStr <- Ascii.pgGetProgressStr opts opts <$> Ascii.Internal.getInfoStats info
  Ascii.setConsoleRegion region pgStr

  future <- Async.async $ start info
  return $ Ascii.ProgressBar info future region
  where
    start info@Ascii.Internal.ProgressBarInfo {..} = do
      c <- readMVar pgCompleted
      unlessDone c $ do
        n <- readChan pgChannel
        _ <- handleMessage info n
        unlessDone (c + n) $ start info
      where
        unlessDone c action | c < Ascii.pgTotal opts = action
        unlessDone _ _ = do
          let fmt = fromMaybe (Ascii.pgFormat opts) (Ascii.pgOnCompletion opts)
          onCompletion <- Ascii.pgGetProgressStr opts opts {Ascii.pgFormat = fmt} <$> Ascii.Internal.getInfoStats info
          Ascii.setConsoleRegion region onCompletion

    handleMessage info n = do
      -- Update the completed tick count
      modifyMVar_ (Ascii.Internal.pgCompleted info) (\c -> return (c + n))
      -- Find and update the current and first tick times:
      stats <- Ascii.Internal.getInfoStats info
      let progressStr = Ascii.Internal.pgGetProgressStr opts opts stats
      Ascii.setConsoleRegion region progressStr

-- | Cancel async updates.
cancelUpdates :: Ascii.ProgressBar -> IO ()
cancelUpdates (Ascii.ProgressBar _ future _) = Async.cancel future

-- | Cancel async updates and clear the console region with the given string.
clearAsciiWith :: Ascii.ProgressBar -> String -> IO ()
clearAsciiWith pg@(Ascii.ProgressBar _ _ region) str = do
  cancelUpdates pg
  Ascii.setConsoleRegion region str
