module Cachix.Client.Daemon.Progress
  ( UploadProgress,
    new,
    complete,
    fail,
    tick,
  )
where

import Cachix.Client.HumanSize (humanSize)
import Control.Retry (RetryStatus (..))
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

new :: Handle -> String -> Int64 -> IO UploadProgress
new hdl path size = do
  isTerminal <- liftIO $ hIsTerminalDevice hdl
  if isTerminal
    then do
      progressBar <- newProgressBar path size
      return $ ProgressBar {..}
    else do
      hPutStr stderr $ uploadStartFallback path size
      return $ FallbackText {..}

complete :: UploadProgress -> IO ()
complete ProgressBar {..} = Ascii.complete progressBar
complete FallbackText {..} = hPutStr stderr $ uploadComplete path size

tick :: UploadProgress -> Int64 -> IO ()
tick ProgressBar {..} deltaBytes = Ascii.tickN progressBar (fromIntegral deltaBytes)
tick _ _ = return ()

fail :: UploadProgress -> IO ()
fail ProgressBar {..} =
  Ascii.setConsoleRegion (Ascii.pgRegion progressBar) $
    uploadFailed path
fail FallbackText {path} = hPutStr stderr $ uploadFailed path

newProgressBar :: String -> Int64 -> IO Ascii.ProgressBar
newProgressBar path size = do
  let hSize = toS $ humanSize $ fromIntegral size
  let barLength = T.length $ T.replace ":percent" "  0%" $ T.replace "[:bar]" "" (toS $ uploadTickBar path hSize)

  liftIO $
    Ascii.newProgressBar
      Ascii.def
        { Ascii.pgTotal = fromIntegral size,
          -- https://github.com/yamadapc/haskell-ascii-progress/issues/24
          Ascii.pgWidth = 20 + barLength,
          Ascii.pgOnCompletion = Just $ uploadComplete path size,
          Ascii.pgFormat = uploadTickBar path hSize
        }

-- retryText :: RetryStatus -> Text
-- retryText retryStatus =
--   if rsIterNumber retryStatus == 0
--     then ""
--     else color Yellow $ "retry #" <> show (rsIterNumber retryStatus) <> " "

-- fallbackUploadProgress :: String -> Int64 -> IO UploadProgress
-- fallbackUploadProgress path size = do
--   let hSize = toS $ humanSize $ fromIntegral size
--   -- we append newline instead of putStrLn due to https://github.com/haskell/text/issues/242

uploadComplete :: String -> Int64 -> String
uploadComplete path size =
  let hSize = toS $ humanSize $ fromIntegral size
   in color Green "✓ " <> path <> " (" <> hSize <> ")"

uploadFailed :: String -> String
uploadFailed path =
  color Red "✗ " <> path

uploadTickBar :: String -> String -> String
uploadTickBar path hSize =
  color Blue "[:bar] " <> toS path <> " (:percent of " <> hSize <> ")"

uploadStartFallback :: String -> Int64 -> String
uploadStartFallback path size =
  let hSize = toS $ humanSize $ fromIntegral size
   in "Pushing " <> path <> " (" <> hSize <> ")\n"

--   -- appendErrText $ retryText retryStatus <> "Pushing " <> path <> " (" <> toS hSize <> ")\n"
