{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Cachix.Client.Push.S3 where

import qualified Cachix.API as API
import Cachix.API.Error
import Cachix.Client.Retry (retryAll)
import Cachix.Client.Servant (cachixClient)
import Cachix.Types.BinaryCache
import qualified Cachix.Types.MultipartUpload as Multipart
import Conduit (MonadResource, MonadUnliftIO)
import Control.DeepSeq (rwhnf)
import Crypto.Hash (Digest, MD5)
import qualified Crypto.Hash as Crypto
import Data.ByteArray.Encoding (Base (..), convertToBase)
import qualified Data.ByteString as BS
import Data.Conduit (ConduitT, await, handleC, yield, ($$+), ($$++), (.|))
import Data.Conduit.ByteString (ChunkSize, chunkStream)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.ConcurrentMap (concurrentMapM_)
import Data.IORef
import Data.List (lookup)
import qualified Data.List.NonEmpty as NonEmpty
import Data.UUID (UUID)
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Conduit
import qualified Network.HTTP.Types.Header as HTTP
import Protolude
import Servant.Auth ()
import Servant.Auth.Client
import qualified Servant.Client as Client
import Servant.Client.Streaming
import Servant.Conduit ()

-- | The size of each uploaded part.
--
-- Common values for S3 are 8MB and 16MB. The minimum is 5MB.
--
-- Lower values will increase HTTP overhead. Some cloud services impose request body limits.
-- For example, Amazon API Gateway caps out at 10MB.
chunkSize :: ChunkSize
chunkSize = 8 * 1024 * 1024

-- | The number of parts to upload concurrently.
-- Speeds up the upload of very large files.
concurrentParts :: Int
concurrentParts = 8

-- | The size of the temporary output buffer.
--
-- Keep this value high to avoid stalling smaller uploads while waiting for a large upload to complete.
-- Each completed upload response is very lightweight.
outputBufferSize :: Int
outputBufferSize = 100

streamUpload ::
  (MonadUnliftIO m, MonadResource m) =>
  ClientEnv ->
  Token ->
  Text ->
  CompressionMethod ->
  (ByteString -> IO ()) ->
  ConduitT
    ByteString
    Void
    m
    (Either SomeException (UUID, Text, Maybe (NonEmpty Multipart.CompletedPart)))
streamUpload env authToken cacheName compressionMethod onUpload = do
  Multipart.CreateMultipartUploadResponse {narId, uploadId} <- createMultipartUpload env authToken cacheName compressionMethod

  handleC (abortMultipartUpload env authToken cacheName narId uploadId) $
    chunkStream (Just chunkSize)
      -- .| CC.mapM (uploadPart env manager authToken cacheName narId uploadId onUpload)
      .| concurrentMapM_ concurrentParts outputBufferSize (uploadPart env manager authToken cacheName narId uploadId onUpload)
      .| completeMultipartUpload narId uploadId
  where
    manager = Client.manager env

uploadPart :: (MonadResource m) => ClientEnv -> Manager -> Token -> Text -> UUID -> Text -> (ByteString -> IO ()) -> (Int, ByteString) -> m (Maybe Multipart.CompletedPart)
uploadPart env manager authToken cacheName narId uploadId onUpload (partNumber, !part) = do
  let partHashMD5 :: Digest MD5 = Crypto.hash part
      contentMD5 :: ByteString = convertToBase Base64 partHashMD5

  let uploadNarPartRequest = API.uploadNarPart cachixClient authToken cacheName narId uploadId partNumber (Multipart.SigningData (decodeUtf8 contentMD5))
  Multipart.UploadPartResponse {uploadUrl} <- liftIO $ withClientM uploadNarPartRequest env escalate

  -- TODO: return from chunker
  let partSize = fromIntegral $ BS.length part

  -- let filePopper :: Handle -> Popper
  --     filePopper h = do
  --       bs <- S.hGetSome h defaultChunkSize
  --       currentPosition <- fromIntegral <$> hTell h
  --       obs $
  --         StreamFileStatus
  --           { fileSize = size
  --           , readSoFar = currentPosition
  --           , thisChunkSize = S.length bs
  --           }
  --       return bs
  --
  --     givesFilePopper :: GivesPopper ()
  --     givesFilePopper k = withBinaryFile path ReadMode $ \h -> do
  --       k (filePopper h)

  -- putText $ show narId <> ": uploading part " <> show partNumber <> " of " <> show partSize <> "\n"
  uploaded <- liftIO $ newIORef 0
  let trackUpload p = do
        currentPos <- atomicModifyIORef' uploaded $ \c -> (c + BS.length p, c + BS.length p)
        liftIO $ putText $ show narId <> ": uploaded " <> show currentPos <> " bytes\n"

  -- let streamPart =
  --       Data.Conduit.yield part
  --         .| CC.takeE 1024
  --         .| CC.iterM trackUpload

  let srcToPopperIO src f = do
        (rsrc0, ()) <- src $$+ return ()
        irsrc <- newIORef rsrc0
        let popper :: IO ByteString
            popper = do
              rsrc <- readIORef irsrc
              (rsrc', mres) <- rsrc $$++ await
              writeIORef irsrc rsrc'
              case mres of
                Nothing -> return BS.empty
                Just bs
                  | BS.null bs -> popper
                  | otherwise -> return bs
        f popper

  let byteStringPopper callback b sink = do
        ioref <- newIORef b
        let getnextchunk = do
              bs <- readIORef ioref
              -- Read 128kb at a time
              let (c, cs) = BS.splitAt 1024 bs
              writeIORef ioref cs
              callback c
              return c
        sink getnextchunk

  -- let requestBodyStream = requestBodySource partSize streamPart
  -- let requestBodyStream = RequestBodyStream partSize (srcToPopperIO part)
  let requestBodyStream = RequestBodyStream partSize (byteStringPopper onUpload part)

  -- or copy srcIOToPopper from
  -- https://hackage.haskell.org/package/http-conduit-2.3.8.3/docs/src/Network.HTTP.Client.Conduit.html#requestBodySource
  -- let requestBodyStream = HTTP.RequestBodyStream partSize partStream

  initialRequest <- liftIO $ HTTP.parseUrlThrow (toS uploadUrl)
  let request =
        initialRequest
          { HTTP.method = "PUT",
            HTTP.requestBody = requestBodyStream,
            HTTP.requestHeaders =
              [ ("Content-Type", "application/octet-stream"),
                ("Content-MD5", contentMD5)
              ]
          }

  response <- liftIO $ HTTP.httpNoBody request manager
  -- putText $ show narId <> ": uploaded part " <> show partNumber <> "\n"
  let eTag = decodeUtf8 <$> lookup HTTP.hETag (HTTP.responseHeaders response)
  -- Strictly evaluate each eTag after uploading each part
  let !_ = rwhnf eTag
  return $ Multipart.CompletedPart partNumber <$> eTag

completeMultipartUpload narId uploadId = do
  parts <- CC.sinkList
  return $ Right (narId, uploadId, sequenceA $ NonEmpty.fromList parts)

abortMultipartUpload env authToken cacheName narId uploadId err = do
  let abortMultipartUploadRequest = API.abortMultipartUpload cachixClient authToken cacheName narId uploadId
  _ <- liftIO $ withClientM abortMultipartUploadRequest env escalate
  return $ Left err

createMultipartUpload :: (MonadIO m) => ClientEnv -> Token -> Text -> CompressionMethod -> ConduitT ByteString Void m Multipart.CreateMultipartUploadResponse
createMultipartUpload env authToken cacheName compressionMethod =
  liftIO $ withClientM createNarRequest env escalate
  where
    createNarRequest = API.createNar cachixClient authToken cacheName (Just compressionMethod)
