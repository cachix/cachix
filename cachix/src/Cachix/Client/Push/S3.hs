{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.Push.UploadProgress
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant (cachixClient)
import Cachix.Types.BinaryCache
import Cachix.Types.MultipartUpload qualified as Multipart
import Conduit (MonadResource, MonadUnliftIO)
import Control.Concurrent.Async (race)
import Control.DeepSeq (rwhnf)
import Crypto.Hash (Digest, MD5)
import Crypto.Hash qualified as Crypto
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString qualified as BS
import Data.Conduit (ConduitT, handleC, (.|))
import Data.Conduit.ByteString (ChunkSize, chunkStream)
import Data.Conduit.Combinators qualified as CC
#if MIN_VERSION_conduit_concurrent_map(0,1,4)
import Data.Conduit.ConcurrentMap (concurrentMapM)
#else
import Data.Conduit.ConcurrentMap (concurrentMapM_)
#endif
import Data.IORef
import Data.List (lookup)
import Data.List.NonEmpty qualified as NonEmpty
import Data.UUID (UUID)
import GHC.Clock (getMonotonicTimeNSec)
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
    compressionMethod :: CompressionMethod,
    -- | Nanoseconds of inactivity before declaring an upload stalled.
    uploadTimeout :: Word64
  }

data UploadMultipartResult = UploadMultipartResult
  { urNarId :: UUID,
    urUploadId :: Text,
    urParts :: Maybe (NonEmpty Multipart.CompletedPart)
  }
  deriving stock (Eq, Show)

-- Disabled due to CPP
{- ORMOLU_DISABLE -}
-- | The size of sub-chunks fed to the HTTP client popper.
-- Small enough that each write() completes quickly on a healthy connection,
-- providing periodic proof of liveness.
subChunkSize :: Int
subChunkSize = 64 * 1024 -- 64KB

-- | Minimum interval between onUpload callback invocations.
emitIntervalNs :: Word64
emitIntervalNs = 200 * 1_000_000 -- 200ms

-- | Interval between watchdog liveness checks.
watchdogPollIntervalUs :: Int
watchdogPollIntervalUs = 30 * 1_000_000 -- 30 seconds

uploadMultipart ::
  forall m.
  (MonadUnliftIO m, MonadResource m) =>
  ClientEnv ->
  Token ->
  Text ->
  (UploadProgress -> IO ()) ->
  UploadMultipartOptions ->
  ConduitT
    ByteString
    Void
    m
    (Either ClientError UploadMultipartResult)
uploadMultipart env authToken cacheName onUploadIO options = do
  createMultipartUpload >>= \case
    Left err -> return $ Left err
    Right (Multipart.CreateMultipartUploadResponse {narId, uploadId}) -> do
      handleC (abortMultipartUpload narId uploadId) $
        chunkStream (Just (chunkSize options))
#if MIN_VERSION_conduit_concurrent_map(0,1,4)
          .| concurrentMapM (numConcurrentChunks options) outputBufferSize (uploadPart narId uploadId)
#else
          .| concurrentMapM_ (numConcurrentChunks options) outputBufferSize (uploadPart narId uploadId)
#endif
          .| completeMultipartUpload narId uploadId
  where
    -- The size of the temporary output buffer.
    --
    -- Keep this value high to avoid stalling smaller uploads while waiting for a large upload to complete.
    -- Each completed upload response is very lightweight.
    outputBufferSize :: Int
    outputBufferSize = 2 * numConcurrentChunks options

    manager = Client.manager env
    timeout = uploadTimeout options

    createMultipartUpload :: ConduitT ByteString Void m (Either ClientError Multipart.CreateMultipartUploadResponse)
    createMultipartUpload = do
      let createNarRequest = API.createNar cachixClient authToken cacheName (Just (compressionMethod options))
      liftIO $ retryHttp $ runClientM createNarRequest env

    uploadPart :: UUID -> Text -> (Int, ByteString) -> m (Maybe Multipart.CompletedPart)
    uploadPart narId uploadId (partNumber, !part) = liftIO $ do
      let partHashMD5 :: Digest MD5 = Crypto.hash part
          contentMD5 :: ByteString = convertToBase Base64 partHashMD5
          pSize = fromIntegral (BS.length part)
          chunk = MultipartChunk {partNumber = partNumber, partSize = pSize}

      let uploadNarPartRequest =
            API.uploadNarPart cachixClient authToken cacheName narId uploadId partNumber (Multipart.SigningData (decodeUtf8 contentMD5))
      Multipart.UploadPartResponse {uploadUrl} <- retryHttp $ withClientM uploadNarPartRequest env escalate

      initialRequest <- HTTP.parseUrlThrow (toS uploadUrl)

      -- Liveness tracking state
      lastEmitRef <- newIORef =<< getMonotonicTimeNSec
      cumulativeRef <- newIORef (0 :: Int64)
      lastEmittedRef <- newIORef (0 :: Int64)

      let popper = mkPopper chunk lastEmitRef cumulativeRef lastEmittedRef part

      let request =
            initialRequest
              { HTTP.method = "PUT",
                HTTP.requestBody = HTTP.RequestBodyStream pSize popper,
                HTTP.requestHeaders =
                  [ ("Content-Type", "application/octet-stream"),
                    ("Content-MD5", contentMD5)
                  ]
              }

      result <- race (watchdog lastEmitRef) (retryHttp $ HTTP.httpNoBody request manager)
      case result of
        Left () -> throwIO $ UploadStalled "Upload stalled: no activity for upload-timeout period"
        Right response -> do
          let eTag = decodeUtf8 <$> lookup HTTP.hETag (HTTP.responseHeaders response)
          -- Strictly evaluate each eTag after uploading each part
          let !_ = rwhnf eTag
          return $ Multipart.CompletedPart partNumber <$> eTag

    mkPopper :: ChunkInfo -> IORef Word64 -> IORef Int64 -> IORef Int64 -> ByteString -> GivesPopper ()
    mkPopper chunk lastEmitRef cumulativeRef lastEmittedRef part needsPopper = do
      remainingRef <- newIORef part
      needsPopper $ do
        remaining <- readIORef remainingRef
        if BS.null remaining
          then do
            -- Flush any remaining progress
            cumulative <- readIORef cumulativeRef
            lastEmitted <- readIORef lastEmittedRef
            when (cumulative > lastEmitted) $ do
              writeIORef lastEmittedRef cumulative
              writeIORef lastEmitRef =<< getMonotonicTimeNSec
              onUploadIO UploadProgress
                { chunkInfo = chunk
                , cumulativeBytes = cumulative
                , bytes = cumulative - lastEmitted
                }
            return BS.empty
          else do
            let (sub, rest) = BS.splitAt subChunkSize remaining
                subLen = fromIntegral (BS.length sub)
            writeIORef remainingRef rest
            cumulative <- atomicModifyIORef' cumulativeRef (\b -> let b' = b + subLen in (b', b'))
            now <- getMonotonicTimeNSec
            lastEmit <- readIORef lastEmitRef
            let pSize = case chunk of
                  MultipartChunk {partSize} -> partSize
                  SingleChunk -> -1
            when (now - lastEmit >= emitIntervalNs || cumulative == pSize) $ do
              writeIORef lastEmitRef now
              lastEmitted <- readIORef lastEmittedRef
              writeIORef lastEmittedRef cumulative
              when (cumulative > lastEmitted) $
                onUploadIO UploadProgress
                  { chunkInfo = chunk
                  , cumulativeBytes = cumulative
                  , bytes = cumulative - lastEmitted
                  }
            return sub

    watchdog :: IORef Word64 -> IO ()
    watchdog lastEmitRef = forever $ do
      threadDelay watchdogPollIntervalUs
      lastEmit <- readIORef lastEmitRef
      now <- getMonotonicTimeNSec
      when (now - lastEmit > timeout) $
        throwIO $ UploadStalled "Upload stalled: no activity for upload-timeout period"

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

{- ORMOLU_ENABLE -}
