{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cachix.Client.Push
  ( -- * Pushing a single path
    pushSingleStorePath
  , PushCache(..)
  , PushStrategy(..)
  , defaultWithXzipCompressor
  , defaultWithXzipCompressorWithLevel

    -- * Pushing a closure of store paths
  , pushClosure
  , mapConcurrentlyBounded
  ) where

import           Crypto.Sign.Ed25519
import qualified Control.Concurrent.QSem       as QSem
import           Control.Concurrent.Async     (mapConcurrently)
import           Control.Monad                ((>=>))
import           Control.Monad.Trans.Resource (ResourceT)
import           Control.Exception.Safe       (MonadMask, throwM)
import           Control.Retry                (recoverAll, RetryStatus, RetryPolicy, constantDelay, limitRetries)
import qualified Data.ByteString.Base64        as B64
import           Data.Conduit
import           Data.Conduit.Process
import           Data.Conduit.Lzma              ( compress )
import           Data.IORef
import qualified Data.Text                      as T
import           Network.HTTP.Types             (status404, status401)
import           Protolude
import           Servant.API
import           Servant.Auth                   ()
import           Servant.Auth.Client
import           Servant.Client.Streaming hiding (ClientError)
import           Servant.Conduit                ()
import           System.Process                 ( readProcess )

import qualified Cachix.Api                    as Api
import           Cachix.Api.Error
import           Cachix.Api.Signing             (fingerprint, passthroughSizeSink, passthroughHashSink)
import qualified Cachix.Types.NarInfoCreate    as Api
import           Cachix.Client.Exception        ( CachixException(..) )
import           Cachix.Client.Servant
import           Cachix.Client.Secrets


data PushCache = PushCache
  { pushCacheName :: Text
  , pushCacheSigningKey :: SigningKey
  , pushCacheToken :: Token
  }
data PushStrategy m r = PushStrategy
  { onAlreadyPresent :: m r -- ^ Called when a path is already in the cache.
  , onAttempt :: RetryStatus -> m ()
  , on401 :: m r
  , onError :: ClientError -> m r
  , onDone :: m r
  , withXzipCompressor :: forall a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
  }

defaultWithXzipCompressor :: forall m a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
defaultWithXzipCompressor = ($ compress (Just 2))

defaultWithXzipCompressorWithLevel :: Int -> forall m a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
defaultWithXzipCompressorWithLevel l = ($ compress (Just l))

pushSingleStorePath
  :: (MonadMask m, MonadIO m)
  => ClientEnv  -- ^ cachix base url, connection manager, see 'Cachix.Client.URI.defaultCachixBaseUrl', 'Servant.Client.mkClientEnv'
  -> PushCache -- ^ details for pushing to cache
  -> PushStrategy m r -- ^ how to report results, (some) errors, and do some things
  -> Text -- ^ store path
  -> m r -- ^ r is determined by the 'PushStrategy'
pushSingleStorePath ce cache cb storePath = retryPath $ \retrystatus -> do

  let (storeHash, storeSuffix) = splitStorePath $ toS storePath
      name = pushCacheName cache

  -- Check if narinfo already exists
  -- TODO: query also cache.nixos.org? server-side?
  res <- liftIO $ (`runClientM` ce) $ Api.narinfoHead
    (cachixBCClient name)
    (pushCacheToken cache)
    (Api.NarInfoC storeHash)
  case res of
    Right NoContent -> onAlreadyPresent cb -- we're done as store path is already in the cache
    Left err | isErr err status404 -> doUpload
             | isErr err status401 -> on401 cb
             | otherwise -> onError cb err
     where
      doUpload = do
        onAttempt cb retrystatus

        narSizeRef <- liftIO $ newIORef 0
        fileSizeRef <- liftIO $ newIORef 0
        narHashRef <- liftIO $ newIORef ("" :: Text)
        fileHashRef <- liftIO $ newIORef ("" :: Text)

        -- stream store path as xz compressed nar file
        let cmd = proc "nix-store" ["--dump", toS storePath]
        -- TODO: print process stderr?
        (ClosedStream, (stdoutStream, closeStdout), ClosedStream, cph) <- liftIO $ streamingProcess cmd
        withXzipCompressor cb $ \xzCompressor -> do
          let stream'
                = stdoutStream
                    .| passthroughSizeSink narSizeRef
                    .| passthroughHashSink narHashRef
                    .| xzCompressor
                    .| passthroughSizeSink fileSizeRef
                    .| passthroughHashSink fileHashRef

          -- for now we need to use letsencrypt domain instead of cloudflare due to its upload limits
          let newClientEnv = ce {
                  baseUrl = (baseUrl ce) { baseUrlHost = toS name <> "." <> baseUrlHost (baseUrl ce)}
                }
          (_ :: NoContent) <- liftIO $ (`withClientM` newClientEnv)
              (Api.createNar (cachixBCStreamingClient name) stream')
              $ escalate >=> \NoContent -> do
                  closeStdout
                  exitcode <- waitForStreamingProcess cph
                  when (exitcode /= ExitSuccess) $ throwM $ NarStreamingError exitcode $ show cmd

                  -- TODO: we're done, so we can leave withClientM. Doing so
                  --       will allow more concurrent requests when the number
                  --       of XZ compressors is limited
                  narSize <- readIORef narSizeRef
                  narHashB16 <- readIORef narHashRef
                  fileHash <- readIORef fileHashRef
                  fileSize <- readIORef fileSizeRef

                  -- TODO: #3: implement using pure haskell
                  narHash <- ("sha256:" <>) . T.strip . toS <$> readProcess "nix-hash" ["--type", "sha256", "--to-base32", toS narHashB16] mempty

                  deriverRaw <- T.strip . toS <$> readProcess "nix-store" ["-q", "--deriver", toS storePath] mempty
                  let deriver = if deriverRaw == "unknown-deriver"
                                then deriverRaw
                                else T.drop 11 deriverRaw

                  references <- sort . T.lines . T.strip . toS <$> readProcess "nix-store" ["-q", "--references", toS storePath] mempty

                  let
                      fp = fingerprint storePath narHash narSize references
                      sig = dsign (signingSecretKey $ pushCacheSigningKey cache) fp
                      nic = Api.NarInfoCreate
                        { cStoreHash = storeHash
                        , cStoreSuffix = storeSuffix
                        , cNarHash = narHash
                        , cNarSize = narSize
                        , cFileSize = fileSize
                        , cFileHash = fileHash
                        , cReferences = fmap (T.drop 11) references
                        , cDeriver = deriver
                        , cSig = toS $ B64.encode $ unSignature sig
                        }

                  escalate $ Api.isNarInfoCreateValid nic

                  -- Upload narinfo with signature
                  escalate <=< (`runClientM` ce) $ Api.createNarinfo
                    (cachixBCClient name)
                    (Api.NarInfoC storeHash)
                    nic
          onDone cb
  where
    -- Retry up to 5 times for each store path.
    -- Catches all exceptions except skipAsyncExceptions
    retryPath :: (MonadIO m, MonadMask m) => (RetryStatus -> m a) -> m a
    retryPath = recoverAll defaultRetryPolicy

    defaultRetryPolicy :: RetryPolicy
    defaultRetryPolicy =
      constantDelay 50000 <> limitRetries 3

-- | Push an entire closure
--
-- Note: 'onAlreadyPresent' will be called less often in the future.
pushClosure
  :: (MonadIO m, MonadMask m)
  => (forall a b. (a -> m b) -> [a] -> m [b])
     -- ^ Traverse paths, responsible for bounding parallel processing of paths
     --
     -- For example: @'mapConcurrentlyBounded' 4@
  -> ClientEnv -- ^ See 'pushSingleStorePath'
  -> PushCache
  -> (Text -> PushStrategy m r)
  -> [Text] -- ^ Initial store paths
  -> m [r] -- ^ Every @r@ per store path of the entire closure of store paths
pushClosure traversal clientEnv pushCache pushStrategy inputStorePaths = do
  
  -- Get the transitive closure of dependencies
  -- TODO: split args if too big
  paths <- liftIO $ T.lines . toS <$> readProcess "nix-store" (fmap toS (["-qR"] <> inputStorePaths)) mempty

  -- TODO: make pool size configurable, on beefier machines this could be doubled
  traversal (\path -> pushSingleStorePath clientEnv pushCache (pushStrategy path) path) paths

mapConcurrentlyBounded :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapConcurrentlyBounded bound action items = do
  qs <- QSem.newQSem bound
  let wrapped x = bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) (action x)
  mapConcurrently wrapped items

-------------------
-- Private terms --

splitStorePath :: Text -> (Text, Text)
splitStorePath storePath =
  (T.take 32 (T.drop 11 storePath), T.drop 44 storePath)
