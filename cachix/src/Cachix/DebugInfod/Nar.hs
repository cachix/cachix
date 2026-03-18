module Cachix.DebugInfod.Nar
  ( unpackNar,
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.Conduit (runConduit, (.|))
import Data.Conduit.Binary qualified as CB
import Data.Conduit.Lzma qualified as Lzma
import Data.Conduit.Zstd qualified as Zstd
import Data.Text qualified as T
import Protolude hiding (toS)
import System.IO (hClose, hFlush)
import System.IO.Error (userError)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
  )

-- | Unpack a (possibly compressed) NAR into a destination directory.
-- Detects compression from the narPath extension (.xz or .zst).
-- Shells out to @nix-store --restore@ for NAR unpacking.
unpackNar :: Text -> LBS.ByteString -> FilePath -> IO ()
unpackNar narPath narData destination = do
  decompressed <- decompressIO narPath narData
  let cp = (proc "nix-store" ["--restore", destination]) {std_in = CreatePipe}
  (Just stdinH, _, _, ph) <- createProcess cp
  LBS.hPut stdinH decompressed
  hFlush stdinH
  hClose stdinH
  exitCode <- waitForProcess ph
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code -> throwIO $ userError $ "nix-store --restore failed with exit code " <> show code

-- | Decompress based on file extension.
decompressIO :: Text -> LBS.ByteString -> IO LBS.ByteString
decompressIO narPath input
  | ".xz" `T.isSuffixOf` narPath = decompressXz input
  | ".zst" `T.isSuffixOf` narPath = decompressZstd input
  | otherwise = pure input

decompressXz :: LBS.ByteString -> IO LBS.ByteString
decompressXz input =
  runConduit $
    CB.sourceLbs input
      .| Lzma.decompress Nothing
      .| CB.sinkLbs

decompressZstd :: LBS.ByteString -> IO LBS.ByteString
decompressZstd input =
  runConduit $
    CB.sourceLbs input
      .| Zstd.decompress
      .| CB.sinkLbs
