module Cachix.Client.Push.UploadProgress
  ( ChunkInfo (..),
    UploadProgress (..),
  )
where

import Protolude

-- | Describes the kind of upload chunk being transferred.
data ChunkInfo
  = -- | A single-part upload (no multipart).
    SingleChunk
  | -- | A part of a multipart upload.
    MultipartChunk
      { partNumber :: Int,
        partSize :: Int64
      }

-- | Progress update for an in-flight upload chunk.
data UploadProgress = UploadProgress
  { -- | The kind of upload chunk.
    chunkInfo :: ChunkInfo,
    -- | Cumulative bytes uploaded for this chunk so far.
    cumulativeBytes :: Int64,
    -- | Bytes uploaded since the last progress update.
    bytes :: Int64
  }
