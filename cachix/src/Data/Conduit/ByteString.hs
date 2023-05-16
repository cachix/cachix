module Data.Conduit.ByteString where

import Conduit (MonadUnliftIO)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString (PS), mallocByteString)
import Data.ByteString.Unsafe (unsafeIndex)
import Data.Conduit (ConduitT, await, yield, (.|))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable (pokeByteOff)
import Protolude hiding (hash, yield)

type ChunkSize = Int

minimumChunkSize :: ChunkSize
minimumChunkSize = 5 * 1024 * 1024

chunkStream :: (MonadUnliftIO m) => Maybe ChunkSize -> ConduitT ByteString (Int, ByteString) m ()
chunkStream mChunkSize = do
  processAndChunkOutputRaw chunkSize
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

data S = S (ForeignPtr Word8) (Ptr Word8) {-# UNPACK #-} !Int

newS :: ChunkSize -> IO S
newS chunkSize = do
  fptr <- mallocByteString chunkSize
  return (S fptr (unsafeForeignPtrToPtr fptr) 0)

processChunk :: ChunkSize -> ByteString -> S -> IO ([ByteString], S)
processChunk chunkSize input =
  loop identity 0
  where
    loop front idxIn s@(S fptr ptr idxOut)
      | idxIn >= BS.length input = return (front [], s)
      | otherwise = do
        pokeByteOff ptr idxOut (unsafeIndex input idxIn)
        let idxOut' = idxOut + 1
            idxIn' = idxIn + 1
        if idxOut' >= chunkSize
          then do
            let bs = PS fptr 0 idxOut'
            s' <- newS chunkSize
            loop (front . (bs :)) idxIn' s'
          else loop front idxIn' (S fptr ptr idxOut')

processAndChunkOutputRaw :: MonadIO m => ChunkSize -> ConduitT ByteString ByteString m ()
processAndChunkOutputRaw chunkSize =
  liftIO (newS chunkSize) >>= loop
  where
    loop s@(S fptr _ len) = do
      mbs <- await
      case mbs of
        Nothing -> yield $ PS fptr 0 len
        Just bs -> do
          (bss, s') <- liftIO $ processChunk chunkSize bs s
          mapM_ yield bss
          loop s'
