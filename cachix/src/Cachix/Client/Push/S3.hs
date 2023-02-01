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
import Conduit (MonadResource, MonadUnliftIO, ResourceT)
import Control.DeepSeq (rwhnf)
import Crypto.Hash (Digest, MD5)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as BS
import Data.Conduit (ConduitT, handleC, (.|))
import Data.Conduit.Binary (sourceLbs)
import qualified Data.Conduit.Combinators as CC
import Data.List (lookup)
import qualified Data.List.NonEmpty as NonEmpty
import Data.UUID (UUID)
import Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import Network.HTTP.Types.URI
import Network.URI
import Protolude
import Servant.API
import Servant.Auth ()
import Servant.Auth.Client
import qualified Servant.Client as Client
import Servant.Client.Streaming
import Servant.Conduit ()
import Servant.Links
import Prelude (String)

partSize :: ChunkSize
partSize = 5 * 1024 * 1024

uploadNarPart = fieldLink API.uploadNarPart

absoluteLinkURI :: BaseUrl -> Link -> URI
absoluteLinkURI burl apil =
  URI
    { uriScheme = fromScheme (baseUrlScheme burl),
      uriAuthority =
        Just $
          nullURIAuth
            { uriUserInfo = mempty,
              uriRegName = baseUrlHost burl,
              uriPort = ':' : show (baseUrlPort burl)
            },
      uriPath = baseUrlPath burl <> "/api/v1/" <> uriPath api,
      uriQuery = uriQuery api,
      uriFragment = uriFragment api
    }
  where
    api = linkURI apil

    fromScheme Https = "https:"
    fromScheme Http = "http:"

streamUpload :: forall m. (MonadUnliftIO m, MonadResource m) => ClientEnv -> Token -> Text -> Maybe CompressionMethod -> ConduitT ByteString Void m ()
streamUpload env authToken cacheName compressionMethod = do
  Multipart.CreateMultipartUploadResponse {narId, uploadId} <- createMultipartUpload

  chunkStream (Just partSize)
    .| CC.mapM (uploadPart narId uploadId)
    .| completeMultipartUpload narId uploadId
    .| CC.sinkNull
  where
    manager = Client.manager env
    baseUrl = Client.baseUrl env

    createMultipartUpload =
      let createNarRequest = API.createNar cachixClient authToken cacheName compressionMethod True
       in liftIO $ withClientM createNarRequest env escalate

    uploadPart :: UUID -> Text -> (Int, ByteString) -> m (Maybe Multipart.CompletedPart)
    uploadPart narId uploadId (partNumber, !part) = do
      -- let partHash = Crypto.hash part :: Digest MD5

      let uploadNarPartRequest = API.uploadNarPart cachixClient authToken cacheName narId uploadId partNumber
      Multipart.UploadPartResponse {uploadUrl} <- liftIO $ withClientM uploadNarPartRequest env escalate

      liftIO $ print uploadUrl

      initialRequest <- liftIO $ HTTP.parseRequest (toS uploadUrl)
      let request =
            initialRequest
              { HTTP.method = "PUT",
                HTTP.requestBody = HTTP.RequestBodyBS part,
                HTTP.requestHeaders =
                  [ ("Content-Type", "application/octet-stream")
                  -- ("Content-MD5", show partHash)
                  ]
              }
      response <- liftIO (HTTP.httpNoBody request manager)
      liftIO $ print (responseStatus response)
      let metag = lookup HTTP.hETag (HTTP.responseHeaders response)

      return $
        case metag of
          Nothing -> Nothing
          Just eTag -> Just $ Multipart.CompletedPart {partNumber, eTag = decodeUtf8 eTag}

    completeMultipartUpload :: UUID -> Text -> ConduitT (Maybe Multipart.CompletedPart) () m ()
    completeMultipartUpload narId uploadId = do
      parts <- CC.sinkList
      let body = Multipart.CompletedMultipartUpload {parts = sequenceA $ NonEmpty.fromList parts}
      print body
      let completeMultipartUploadRequest = API.completeNarUpload cachixClient authToken cacheName narId uploadId body
      _ <- liftIO $ withClientM completeMultipartUploadRequest env escalate
      return ()
