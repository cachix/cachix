module Cachix.Client.Push.Conduit where

import Conduit (MonadUnliftIO)
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (byteStringCopy, runBuilder)
import Data.ByteString.Internal (ByteString (PS))
import Data.Conduit (ConduitT, await, yield, (.|))
import qualified Data.Conduit.Combinators as CC
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, plusForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Protolude hiding (hash, yield)

type ChunkSize = Int

minimumChunkSize :: ChunkSize
minimumChunkSize = 6 * 1024 * 1024

chunkStream :: (MonadUnliftIO m) => Maybe ChunkSize -> ConduitT ByteString (Int, ByteString) m ()
chunkStream mChunkSize = do
  buffer <- liftIO $ allocBuffer chunkSize
  unsafeWriteChunksToBuffer buffer
    .| CC.map (bufferToByteString buffer)
    .| enumerateConduit
  where
    chunkSize :: ChunkSize
    chunkSize = maybe minimumChunkSize (max minimumChunkSize) mChunkSize

    -- count from 1
    enumerateConduit :: (Monad m) => ConduitT a (Int, a) m ()
    enumerateConduit = loop 1
      where
        loop i = await >>= maybe (return ()) (go i)
        go i x = do
          yield (i, x)
          loop (i + 1)
    {-# INLINE enumerateConduit #-}

-- The number of bytes remaining in a buffer, and the pointer that backs it.
data Buffer = Buffer {remaining :: !Int, _fptr :: !(ForeignPtr Word8)}

data PutResult
  = Ok Buffer -- Didn't fill the buffer, updated buffer.
  | Full ByteString -- Buffer is full, the unwritten remaining string.

data BufferResult = FullBuffer | Incomplete Int

-- Accepts @ByteString@s and writes them into @Buffer@. When the buffer is full,
-- @FullBuffer@ is emitted. If there is no more input, @Incomplete@ is emitted with
-- the number of bytes remaining in the buffer.
unsafeWriteChunksToBuffer :: MonadIO m => Buffer -> ConduitT ByteString BufferResult m ()
unsafeWriteChunksToBuffer buffer0 = awaitLoop buffer0
  where
    awaitLoop buf =
      await
        >>= maybe
          (yield $ Incomplete $ remaining buf)
          ( liftIO . putBuffer buf >=> \case
              Full next -> yield FullBuffer *> chunkLoop buffer0 next
              Ok buf' -> awaitLoop buf'
          )

    -- Handle inputs which are larger than the chunkSize
    chunkLoop buf =
      liftIO . putBuffer buf >=> \case
        Full next -> yield FullBuffer *> chunkLoop buffer0 next
        Ok buf' -> awaitLoop buf'

bufferToByteString :: Buffer -> BufferResult -> ByteString
bufferToByteString (Buffer bufSize fptr) FullBuffer = PS fptr 0 bufSize
bufferToByteString (Buffer bufSize fptr) (Incomplete remaining) = PS fptr 0 (bufSize - remaining)

allocBuffer :: Int -> IO Buffer
allocBuffer chunkSize = Buffer chunkSize <$> mallocForeignPtrBytes chunkSize

putBuffer :: Buffer -> ByteString -> IO PutResult
putBuffer buffer bs
  | BS.length bs <= remaining buffer =
    Ok <$> unsafeWriteBuffer buffer bs
  | otherwise = do
    let (remainder, rest) = BS.splitAt (remaining buffer) bs
    Full rest <$ unsafeWriteBuffer buffer remainder

-- The length of the bytestring must be less than or equal to the number
-- of bytes remaining.
unsafeWriteBuffer :: Buffer -> ByteString -> IO Buffer
unsafeWriteBuffer (Buffer remaining fptr) bs = do
  let ptr = unsafeForeignPtrToPtr fptr
      len = BS.length bs
  _ <- runBuilder (byteStringCopy bs) ptr remaining
  pure $ Buffer (remaining - len) (plusForeignPtr fptr len)
