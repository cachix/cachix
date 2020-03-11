{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.Client.Push
  ( -- * Pushing a single path
    pushSingleStorePath,
    PushCache (..),
    PushStrategy (..),
    defaultWithXzipCompressor,
    defaultWithXzipCompressorWithLevel,

    -- * Pushing a closure of store paths
    pushClosure,
    mapConcurrentlyBounded,
  )
where

import qualified Cachix.Api as Api
import Cachix.Api.Error
import Cachix.Api.Signing (fingerprint, passthroughHashSink, passthroughHashSinkB16, passthroughSizeSink)
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.Secrets
import Cachix.Client.Servant
import Cachix.Client.Store (Store)
import qualified Cachix.Client.Store as Store
import qualified Cachix.Types.NarInfoCreate as Api
import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Concurrent.QSem as QSem
import Control.Exception.Safe (MonadMask, throwM)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Retry (RetryPolicy, RetryStatus, constantDelay, limitRetries, recoverAll)
import Crypto.Sign.Ed25519
import qualified Data.ByteString.Base64 as B64
import Data.Conduit
import Data.Conduit.Lzma (compress)
import Data.Conduit.Process
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Text as T
import Network.HTTP.Types (status401, status404)
import Protolude
import Servant.API
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client.Streaming hiding (ClientError)
import Servant.Conduit ()
import qualified System.Nix.Base32

data PushCache
  = PushCache
      { pushCacheName :: Text,
        pushCacheSigningKey :: SigningKey,
        pushCacheToken :: Token
      }

data PushStrategy m r
  = PushStrategy
      { -- | Called when a path is already in the cache.
        onAlreadyPresent :: m r,
        onAttempt :: RetryStatus -> Int64 -> m (),
        on401 :: m r,
        onError :: ClientError -> m r,
        onDone :: m r,
        withXzipCompressor :: forall a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
      }

defaultWithXzipCompressor :: forall m a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
defaultWithXzipCompressor = ($ compress (Just 2))

defaultWithXzipCompressorWithLevel :: Int -> forall m a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
defaultWithXzipCompressorWithLevel l = ($ compress (Just l))

pushSingleStorePath ::
  (MonadMask m, MonadIO m) =>
  -- | cachix base url, connection manager, see 'Cachix.Client.URI.defaultCachixBaseUrl', 'Servant.Client.mkClientEnv'
  ClientEnv ->
  Store ->
  -- | details for pushing to cache
  PushCache ->
  -- | how to report results, (some) errors, and do some things
  PushStrategy m r ->
  -- | store path
  Text ->
  -- | r is determined by the 'PushStrategy'
  m r
pushSingleStorePath clientEnv _store cache cb storePath = retryAll $ \retrystatus -> do
  let storeHash = fst $ splitStorePath $ toS storePath
      name = pushCacheName cache
  -- Check if narinfo already exists
  -- TODO: query also cache.nixos.org? server-side?
  res <-
    liftIO $ (`runClientM` clientEnv) $
      Api.narinfoHead
        (cachixBCClient name)
        (pushCacheToken cache)
        (Api.NarInfoC storeHash)
  case res of
    Right NoContent -> onAlreadyPresent cb -- we're done as store path is already in the cache
    Left err
      | isErr err status404 -> uploadStorePath clientEnv _store cache cb storePath retrystatus
      | isErr err status401 -> on401 cb
      | otherwise -> onError cb err

uploadStorePath ::
  (MonadMask m, MonadIO m) =>
  -- | cachix base url, connection manager, see 'Cachix.Client.URI.defaultCachixBaseUrl', 'Servant.Client.mkClientEnv'
  ClientEnv ->
  Store ->
  -- | details for pushing to cache
  PushCache ->
  -- | how to report results, (some) errors, and do some things
  PushStrategy m r ->
  -- | store path
  Text ->
  RetryStatus ->
  -- | r is determined by the 'PushStrategy'
  m r
uploadStorePath clientEnv store cache cb storePath retrystatus = do
  let (storeHash, storeSuffix) = splitStorePath $ toS storePath
      name = pushCacheName cache
  narSizeRef <- liftIO $ newIORef 0
  fileSizeRef <- liftIO $ newIORef 0
  narHashRef <- liftIO $ newIORef ("" :: ByteString)
  fileHashRef <- liftIO $ newIORef ("" :: ByteString)
  normalized <- liftIO $ Store.followLinksToStorePath store $ toS storePath
  pathinfo <- liftIO $ Store.queryPathInfo store normalized
  -- stream store path as xz compressed nar file
  let cmd = proc "nix-store" ["--dump", toS storePath]
      storePathSize :: Int64
      storePathSize = Store.validPathInfoNarSize pathinfo
  onAttempt cb retrystatus storePathSize
  (ClosedStream, stdoutStream, Inherited, cph) <- liftIO $ streamingProcess cmd
  withXzipCompressor cb $ \xzCompressor -> do
    let stream' =
          stdoutStream
            .| passthroughSizeSink narSizeRef
            .| passthroughHashSink narHashRef
            .| xzCompressor
            .| passthroughSizeSink fileSizeRef
            .| passthroughHashSinkB16 fileHashRef
    let subdomain =
          -- TODO: multipart
          if (fromIntegral storePathSize / (1024 * 1024)) > 100
            then "api"
            else toS name
        newClientEnv =
          clientEnv
            { baseUrl = (baseUrl clientEnv) {baseUrlHost = subdomain <> "." <> baseUrlHost (baseUrl clientEnv)}
            }
    (_ :: NoContent) <-
      liftIO
        $ (`withClientM` newClientEnv)
          (Api.createNar (cachixBCStreamingClient name) stream')
        $ escalate
          >=> \NoContent -> do
            exitcode <- waitForStreamingProcess cph
            when (exitcode /= ExitSuccess) $ throwM $ NarStreamingError exitcode $ show cmd
            return NoContent
    (_ :: NoContent) <- liftIO $ do
      narSize <- readIORef narSizeRef
      narHash <- ("sha256:" <>) . System.Nix.Base32.encode <$> readIORef narHashRef
      narHashNix <- Store.validPathInfoNarHash pathinfo
      when (narHash /= toS narHashNix) $ throwM $ NarHashMismatch "Nar hash mismatch between nix-store --dump and nix db"
      fileHash <- readIORef fileHashRef
      fileSize <- readIORef fileSizeRef
      deriver <- Store.validPathInfoDeriver pathinfo
      referencesPathSet <- Store.validPathInfoReferences pathinfo
      references <- sort <$> Store.traversePathSet (pure . toS) referencesPathSet
      let fp = fingerprint storePath narHash narSize references
          sig = dsign (signingSecretKey $ pushCacheSigningKey cache) fp
          nic =
            Api.NarInfoCreate
              { Api.cStoreHash = storeHash,
                Api.cStoreSuffix = storeSuffix,
                Api.cNarHash = narHash,
                Api.cNarSize = narSize,
                Api.cFileSize = fileSize,
                Api.cFileHash = toS fileHash,
                Api.cReferences = fmap (T.drop 11) references,
                Api.cDeriver = T.drop 11 (toS deriver),
                Api.cSig = toS $ B64.encode $ unSignature sig
              }
      escalate $ Api.isNarInfoCreateValid nic
      -- Upload narinfo with signature
      escalate <=< (`runClientM` clientEnv) $
        Api.createNarinfo
          (cachixBCClient name)
          (Api.NarInfoC storeHash)
          nic
    onDone cb

-- Catches all exceptions except skipAsyncExceptions
retryAll :: (MonadIO m, MonadMask m) => (RetryStatus -> m a) -> m a
retryAll = recoverAll defaultRetryPolicy
  where
    defaultRetryPolicy :: RetryPolicy
    defaultRetryPolicy =
      constantDelay 50000 <> limitRetries 3

-- | Push an entire closure
--
-- Note: 'onAlreadyPresent' will be called less often in the future.
pushClosure ::
  (MonadIO m, MonadMask m) =>
  -- | Traverse paths, responsible for bounding parallel processing of paths
  --
  -- For example: @'mapConcurrentlyBounded' 4@
  (forall a b. (a -> m b) -> [a] -> m [b]) ->
  -- | See 'pushSingleStorePath'
  ClientEnv ->
  Store ->
  PushCache ->
  (Text -> PushStrategy m r) ->
  -- | Initial store paths
  [Text] ->
  -- | Every @r@ per store path of the entire closure of store paths
  m [r]
pushClosure traversal clientEnv store pushCache pushStrategy inputStorePaths = do
  -- Get the transitive closure of dependencies
  paths <-
    liftIO $ do
      inputs <- Store.newEmptyPathSet
      for_ inputStorePaths $ \path -> do
        normalized <- Store.followLinksToStorePath store (encodeUtf8 path)
        Store.addToPathSet normalized inputs
      closure <- Store.computeFSClosure store Store.defaultClosureParams inputs
      Store.traversePathSet (pure . toSL) closure
  -- Check what store paths are missing
  -- TODO: query also cache.nixos.org? server-side?
  missingHashesList <-
    retryAll $ \_ ->
      escalate
        =<< liftIO
          ( (`runClientM` clientEnv) $
              Api.narinfoBulk
                (cachixBCClient (pushCacheName pushCache))
                (pushCacheToken pushCache)
                (fst . splitStorePath <$> paths)
          )
  let missingHashes = Set.fromList missingHashesList
      missingPaths = filter (\path -> Set.member (fst (splitStorePath path)) missingHashes) paths
  -- TODO: make pool size configurable, on beefier machines this could be doubled
  traversal (\path -> retryAll $ \retrystatus -> uploadStorePath clientEnv store pushCache (pushStrategy path) path retrystatus) missingPaths

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
