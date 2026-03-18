module Cachix.DebugInfod.Nar
  ( unpackNar,
  )
where

import Control.Concurrent.Async (wait, withAsync)
import Data.ByteString qualified as BS
import Data.Conduit (ConduitT, awaitForever, runConduit, yield, (.|))
import Data.Conduit.Binary qualified as CB
import Data.Conduit.Lzma qualified as Lzma
import Data.Conduit.Zstd qualified as Zstd
import Data.Text qualified as T
import GHC.IO.Exception (userError)
import Protolude hiding (yield)
import System.Directory (removeDirectoryRecursive)
import System.IO (hClose)
import System.Nix.Nar (narEffectsIO, unpackNarIO)
import System.Posix.IO (createPipe, fdToHandle)

-- | Unpack a (possibly compressed) NAR from a conduit source into a destination directory.
-- Detects compression from the narPath extension (.xz or .zst).
-- Uses hnix-store-nar to restore the NAR contents.
unpackNar :: Text -> ConduitT () BS.ByteString IO () -> FilePath -> IO ()
unpackNar narPath source destination = do
  -- unpackNarIO requires the destination to not exist.
  -- The caller (Cache.fetchOrGet) creates it as a directory, so remove it first.
  removeDirectoryRecursive destination
  (readFd, writeFd) <- createPipe
  readH <- fdToHandle readFd
  let restoreThread = do
        result <-
          unpackNarIO narEffectsIO readH destination
            `finally` hClose readH
        case result of
          Left err -> throwIO (userError $ "NAR unpack failed: " <> err)
          Right () -> pure ()
  withAsync restoreThread $ \restoreAsync -> do
    writeH <- fdToHandle writeFd
    runConduit (source .| decompressConduit narPath .| CB.sinkHandle writeH)
      `finally` hClose writeH
    wait restoreAsync

-- | Decompress conduit based on file extension.
decompressConduit :: Text -> ConduitT BS.ByteString BS.ByteString IO ()
decompressConduit narPath
  | ".xz" `T.isSuffixOf` narPath = Lzma.decompress Nothing
  | ".zst" `T.isSuffixOf` narPath = Zstd.decompress
  | otherwise = awaitForever yield
