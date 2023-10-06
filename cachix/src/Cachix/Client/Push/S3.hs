{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Cachix.Client.Push.S3 where

import qualified Cachix.API as API
import Cachix.API.Error
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant (cachixClient)
import Cachix.Types.BinaryCache
import qualified Cachix.Types.MultipartUpload as Multipart
import Conduit (MonadResource, MonadUnliftIO)
import Control.DeepSeq (rwhnf)
import Crypto.Hash (Digest, MD5)
import qualified Crypto.Hash as Crypto
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.Conduit (ConduitT, handleC, (.|))
import Data.Conduit.ByteString (ChunkSize, chunkStream)
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.ConcurrentMap (concurrentMapM_)
import Data.List (lookup)
import qualified Data.List.NonEmpty as NonEmpty
import Data.UUID (UUID)
import Network.HTTP.Client as HTTP
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
  forall m.
  (MonadUnliftIO m, MonadResource m) =>
  ClientEnv ->
  Token ->
  Text ->
  CompressionMethod ->
  ConduitT
    ByteString
    Void
    m
    (Either SomeException (UUID, Text, Maybe (NonEmpty Multipart.CompletedPart)))
streamUpload env authToken cacheName compressionMethod = do
  Multipart.CreateMultipartUploadResponse {narId, uploadId} <- createMultipartUpload

  handleC (abortMultipartUpload narId uploadId) $
    chunkStream (Just chunkSize)
      .| concurrentMapM_ concurrentParts outputBufferSize (uploadPart narId uploadId)
      .| completeMultipartUpload narId uploadId
  where
    manager = Client.manager env

    createMultipartUpload :: ConduitT ByteString Void m Multipart.CreateMultipartUploadResponse
    createMultipartUpload = do
      let createNarRequest = API.createNar cachixClient authToken cacheName (Just compressionMethod)
      liftIO $ retryHttp $ withClientM createNarRequest env escalate

    uploadPart :: UUID -> Text -> (Int, ByteString) -> m (Maybe Multipart.CompletedPart)
    uploadPart narId uploadId (partNumber, !part) = do
      let partHashMD5 :: Digest MD5 = Crypto.hash part
          contentMD5 :: ByteString = convertToBase Base64 partHashMD5

      let uploadNarPartRequest =
            API.uploadNarPart cachixClient authToken cacheName narId uploadId partNumber (Multipart.SigningData (decodeUtf8 contentMD5))
      Multipart.UploadPartResponse {uploadUrl} <- liftIO $ retryHttp $ withClientM uploadNarPartRequest env escalate

      initialRequest <- liftIO $ HTTP.parseUrlThrow (toS uploadUrl)
      let request =
            initialRequest
              { HTTP.method = "PUT",
                HTTP.requestBody = HTTP.RequestBodyBS part,
                HTTP.requestHeaders =
                  [ ("Content-Type", "application/octet-stream"),
                    ("Content-MD5", contentMD5)
                  ]
              }

      response <- liftIO $ retryHttp $ HTTP.httpNoBody request manager
      let eTag = decodeUtf8 <$> lookup HTTP.hETag (HTTP.responseHeaders response)
      -- Strictly evaluate each eTag after uploading each part
      let !_ = rwhnf eTag
      return $ Multipart.CompletedPart partNumber <$> eTag

    completeMultipartUpload narId uploadId = do
      parts <- CC.sinkList
      return $ Right (narId, uploadId, sequenceA $ NonEmpty.fromList parts)

    abortMultipartUpload narId uploadId err = do
      let abortMultipartUploadRequest = API.abortMultipartUpload cachixClient authToken cacheName narId uploadId
      _ <- liftIO $ retryHttp $ withClientM abortMultipartUploadRequest env escalate
      return $ Left err
