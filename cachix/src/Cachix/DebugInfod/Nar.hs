module Cachix.DebugInfod.Nar
  ( unpackNar,
  )
where

import Data.ByteString qualified as BS
import Data.Conduit (ConduitT, awaitForever, runConduit, yield, (.|))
import Data.Conduit.Binary qualified as CB
import Data.Conduit.Lzma qualified as Lzma
import Data.Conduit.Zstd qualified as Zstd
import Data.Text qualified as T
import Protolude hiding (toS, yield)
import System.Directory (removeDirectoryRecursive)
import System.IO (hClose, hFlush)
import System.IO.Error (userError)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
  )

-- | Unpack a (possibly compressed) NAR from a conduit source into a destination directory.
-- Detects compression from the narPath extension (.xz or .zst).
-- Shells out to @nix-store --restore@ for NAR unpacking.
unpackNar :: Text -> ConduitT () BS.ByteString IO () -> FilePath -> IO ()
unpackNar narPath source destination = do
  -- nix-store --restore requires the destination to not exist.
  -- The caller (Cache.fetchOrGet) creates it as a directory, so remove it first.
  removeDirectoryRecursive destination
  let cp = (proc "nix-store" ["--restore", destination]) {std_in = CreatePipe}
  (Just stdinH, _, _, ph) <- createProcess cp
  runConduit $ source .| decompressConduit narPath .| CB.sinkHandle stdinH
  hFlush stdinH
  hClose stdinH
  exitCode <- waitForProcess ph
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure code -> throwIO $ userError $ "nix-store --restore failed with exit code " <> show code

-- | Decompress conduit based on file extension.
decompressConduit :: Text -> ConduitT BS.ByteString BS.ByteString IO ()
decompressConduit narPath
  | ".xz" `T.isSuffixOf` narPath = Lzma.decompress Nothing
  | ".zst" `T.isSuffixOf` narPath = Zstd.decompress
  | otherwise = awaitForever yield
