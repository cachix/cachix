{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cachix.Client.Push.S3 where

import qualified Cachix.API as API
import Cachix.API.Error
import Cachix.Client.Push.Conduit (ChunkSize, chunkStream)
import Cachix.Client.Servant (cachixClient)
import Cachix.Types.BinaryCache
import qualified Cachix.Types.MultipartUpload as Multipart
import Conduit (MonadResource, MonadUnliftIO)
import Control.DeepSeq (rwhnf)
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as Crypto
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.Combinators as CC
import Data.Conduit.ConcurrentMap (concurrentMapM_)
import qualified Data.IORef as IORef
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

partSize :: ChunkSize
partSize = 8 * 1024 * 1024

streamUpload ::
  forall m.
  (MonadUnliftIO m, MonadResource m) =>
  ClientEnv ->
  Token ->
  Text ->
  CompressionMethod ->
  IORef.IORef Integer ->
  IORef.IORef ByteString ->
  ConduitT ByteString Void m ()
streamUpload env authToken cacheName compressionMethod fileSizeRef fileHashRef = do
  Multipart.CreateMultipartUploadResponse {narId, uploadId} <- createMultipartUpload

  chunkStream (Just partSize)
    .| concurrentMapM_ 10 5 (uploadPart narId uploadId)
    .| completeMultipartUpload narId uploadId
    .| CC.sinkNull
  where
    manager = Client.manager env

    createMultipartUpload :: ConduitT ByteString Void m Multipart.CreateMultipartUploadResponse
    createMultipartUpload =
      liftIO $ withClientM createNarRequest env escalate
      where
        createNarRequest = API.createNar cachixClient authToken cacheName (Just compressionMethod) True

    uploadPart :: UUID -> Text -> (Int, ByteString) -> m (Maybe Multipart.CompletedPart)
    uploadPart narId uploadId (partNumber, !part) = do
      -- TODO: we could prefetch the upload URL beforehand
      let uploadNarPartRequest = API.uploadNarPart cachixClient authToken cacheName narId uploadId partNumber
      Multipart.UploadPartResponse {uploadUrl} <- liftIO $ withClientM uploadNarPartRequest env escalate

      let partHash :: Digest SHA256 = Crypto.hash part
      initialRequest <- liftIO $ HTTP.parseUrlThrow (toS uploadUrl)
      let request =
            initialRequest
              { HTTP.method = "PUT",
                HTTP.requestBody = HTTP.RequestBodyBS part,
                HTTP.requestHeaders =
                  [ ("Content-Type", "application/octet-stream")
                  ]
              }
      response <- liftIO $ HTTP.httpNoBody request manager
      let eTag = decodeUtf8 <$> lookup HTTP.hETag (HTTP.responseHeaders response)
      -- Strictly evaluate each eTag after uploading each part
      let !_ = rwhnf eTag
      return $ Multipart.CompletedPart partNumber (show partHash) <$> eTag

    completeMultipartUpload :: UUID -> Text -> ConduitT (Maybe Multipart.CompletedPart) () m ()
    completeMultipartUpload narId uploadId = do
      parts <- CC.sinkList
      fileHash <- liftIO $ IORef.readIORef fileHashRef
      fileSize <- liftIO $ IORef.readIORef fileSizeRef
      let body =
            Multipart.CompletedMultipartUpload
              { parts = sequenceA $ NonEmpty.fromList parts,
                fileHash = decodeUtf8 fileHash,
                fileSize = fileSize
              }
      let completeMultipartUploadRequest = API.completeNarUpload cachixClient authToken cacheName narId uploadId body
      _ <- liftIO $ withClientM completeMultipartUploadRequest env escalate
      return ()
