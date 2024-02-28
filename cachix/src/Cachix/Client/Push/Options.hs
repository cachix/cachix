module Cachix.Client.Push.Options
  ( PushOptions (..),
    defaultPushOptions,
    defaultCompressionMethod,
    defaultCompressionLevel,
    defaultChunkSize,
    minChunkSize,
    maxChunkSize,
    defaultNumConcurrentChunks,
    defaultOmitDeriver,
  )
where

import qualified Cachix.Types.BinaryCache as BinaryCache
import Data.Conduit.ByteString (ChunkSize)
import Protolude

-- | Options to configure how paths are pushed to the binary cache.
data PushOptions = PushOptions
  { -- | The compression method to use.
    compressionMethod :: BinaryCache.CompressionMethod,
    -- | The compression level to use.
    compressionLevel :: Int,
    -- | The chunk size to use.
    chunkSize :: ChunkSize,
    -- | The number of chunks to upload concurrently.
    numConcurrentChunks :: Int,
    -- | Whether to omit the deriver from the narinfo.
    omitDeriver :: Bool
  }
  deriving stock (Eq, Show)

defaultPushOptions :: PushOptions
defaultPushOptions =
  PushOptions
    { compressionMethod = defaultCompressionMethod,
      compressionLevel = defaultCompressionLevel,
      chunkSize = defaultChunkSize,
      numConcurrentChunks = defaultNumConcurrentChunks,
      omitDeriver = defaultOmitDeriver
    }

defaultCompressionMethod :: BinaryCache.CompressionMethod
defaultCompressionMethod = BinaryCache.ZSTD

defaultCompressionLevel :: Int
defaultCompressionLevel = 2

defaultChunkSize :: ChunkSize
defaultChunkSize = 32 * 1024 * 1024 -- 32MiB

-- | The minimum size of a chunk.
-- Determined by our upstream API constraints.
minChunkSize :: ChunkSize
minChunkSize = 5 * 1024 * 1024 -- 5MiB

-- | The maximum size of a chunk.
-- Determined by our upstream API constraints.
maxChunkSize :: ChunkSize
maxChunkSize = 5 * 1024 * 1024 * 1024 -- 5GiB

defaultNumConcurrentChunks :: Int
defaultNumConcurrentChunks = 4

defaultOmitDeriver :: Bool
defaultOmitDeriver = False
