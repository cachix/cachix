{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

module Cachix.Client.Push.S3
  ( UploadMultipartResult (..),
    UploadMultipartOptions (..),
    uploadMultipart,
  )
where

import Cachix.API qualified as API
import Cachix.API.Error
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant (cachixClient)
import Cachix.Types.BinaryCache
import Cachix.Types.MultipartUpload qualified as Multipart
import Conduit (MonadResource, MonadUnliftIO)
import Control.DeepSeq (rwhnf)
import Crypto.Hash (Digest, MD5)
import Crypto.Hash qualified as Crypto
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.Conduit (ConduitT, handleC, (.|))
import Data.Conduit.ByteString (ChunkSize, chunkStream)
import Data.Conduit.Combinators qualified as CC
import Data.Conduit.ConcurrentMap (concurrentMapM_)
import Data.List (lookup)
import Data.List.NonEmpty qualified as NonEmpty
import Data.UUID (UUID)
import Network.HTTP.Client as HTTP
import Network.HTTP.Types.Header qualified as HTTP
import Protolude
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client qualified as Client
import Servant.Client.Streaming
import Servant.Conduit ()

data UploadMultipartOptions = UploadMultipartOptions
  { numConcurrentChunks :: Int,
    chunkSize :: ChunkSize,
    compressionMethod :: CompressionMethod
  }

data UploadMultipartResult = UploadMultipartResult
  { urNarId :: UUID,
    urUploadId :: Text,
    urParts :: Maybe (NonEmpty Multipart.CompletedPart)
  }
  deriving stock (Eq, Show)

uploadMultipart ::
  forall m.
  (MonadUnliftIO m, MonadResource m) =>
  ClientEnv ->
  Token ->
  Text ->
  UploadMultipartOptions ->
  ConduitT
    ByteString
    Void
    m
    (Either ClientError UploadMultipartResult)
uploadMultipart env authToken cacheName options = do
  createMultipartUpload >>= \case
    Left err -> return $ Left err
    Right (Multipart.CreateMultipartUploadResponse {narId, uploadId}) -> do
      handleC (abortMultipartUpload narId uploadId) $
        chunkStream (Just (chunkSize options))
          .| concurrentMapM_ (numConcurrentChunks options) outputBufferSize (uploadPart narId uploadId)
          .| completeMultipartUpload narId uploadId
  where
    -- The size of the temporary output buffer.
    --
    -- Keep this value high to avoid stalling smaller uploads while waiting for a large upload to complete.
    -- Each completed upload response is very lightweight.
    outputBufferSize :: Int
    outputBufferSize = 2 * numConcurrentChunks options

    manager = Client.manager env

    createMultipartUpload :: ConduitT ByteString Void m (Either ClientError Multipart.CreateMultipartUploadResponse)
    createMultipartUpload = do
      let createNarRequest = API.createNar cachixClient authToken cacheName (Just (compressionMethod options))
      liftIO $ retryHttp $ runClientM createNarRequest env

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
      return $
        Right $
          UploadMultipartResult
            { urNarId = narId,
              urUploadId = uploadId,
              urParts = sequenceA (NonEmpty.fromList parts)
            }

    abortMultipartUpload narId uploadId err = do
      let abortMultipartUploadRequest = API.abortMultipartUpload cachixClient authToken cacheName narId uploadId
      _ <- liftIO $ retryHttp $ withClientM abortMultipartUploadRequest env escalate
      return $ Left err
