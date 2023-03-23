{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{- This is a standalone module so it shouldn't depend on any CLI state like Env -}
module Cachix.Client.Push
  ( -- * Pushing a single path
    pushSingleStorePath,
    uploadStorePath,
    PushParams (..),
    PushSecret (..),
    PushStrategy (..),
    defaultWithXzipCompressor,
    defaultWithXzipCompressorWithLevel,
    defaultWithZstdCompressor,
    defaultWithZstdCompressorWithLevel,
    findPushSecret,

    -- * Pushing a closure of store paths
    pushClosure,
    getMissingPathsForClosure,
    mapConcurrentlyBounded,
  )
where

import qualified Cachix.API as API
import Cachix.API.Error
import Cachix.API.Signing (fingerprint, passthroughHashSink, passthroughHashSinkB16, passthroughSizeSink)
import qualified Cachix.Client.Config as Config
import Cachix.Client.Exception (CachixException (..))
import qualified Cachix.Client.Push.S3 as Push.S3
import Cachix.Client.Retry (retryAll)
import Cachix.Client.Secrets
import Cachix.Client.Servant
import qualified Cachix.Client.Store as Store
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Cachix.Types.MultipartUpload as Multipart
import qualified Cachix.Types.NarInfoCreate as Api
import qualified Cachix.Types.NarInfoHash as NarInfoHash
import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Concurrent.QSem as QSem
import Control.Exception.Safe (MonadMask, throwM)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Retry (RetryStatus)
import Crypto.Sign.Ed25519
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.Conduit
import qualified Data.Conduit.Lzma as Lzma (compress)
import qualified Data.Conduit.Zstd as Zstd (compress)
import Data.IORef
import qualified Data.Set as Set
import Data.String.Here
import qualified Data.Text as T
import Network.HTTP.Types (status401, status404)
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import System.Environment (lookupEnv)
import qualified System.Nix.Base32
import System.Nix.Nar

data PushSecret
  = PushToken Token
  | PushSigningKey Token SigningKey

data PushParams m r = PushParams
  { pushParamsName :: Text,
    pushParamsSecret :: PushSecret,
    -- | how to report results, (some) errors, and do some things
    pushParamsStrategy :: Store.StorePath -> PushStrategy m r,
    -- | cachix base url, connection manager, see 'Cachix.Client.URI.defaultCachixBaseUrl', 'Servant.Client.mkClientEnv'
    pushParamsClientEnv :: ClientEnv,
    pushParamsStore :: Store.Store
  }

data PushStrategy m r = PushStrategy
  { -- | Called when a path is already in the cache.
    onAlreadyPresent :: m r,
    onAttempt :: RetryStatus -> Int64 -> m (),
    on401 :: ClientError -> m r,
    onError :: ClientError -> m r,
    onDone :: m r,
    compressionMethod :: BinaryCache.CompressionMethod,
    compressionLevel :: Int,
    omitDeriver :: Bool
  }

defaultWithXzipCompressor :: forall m a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
defaultWithXzipCompressor = ($ Lzma.compress (Just 2))

defaultWithXzipCompressorWithLevel :: Int -> forall m a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
defaultWithXzipCompressorWithLevel l = ($ Lzma.compress (Just l))

defaultWithZstdCompressor :: forall m a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
defaultWithZstdCompressor = ($ Zstd.compress 8)

defaultWithZstdCompressorWithLevel :: Int -> forall m a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
defaultWithZstdCompressorWithLevel l = ($ Zstd.compress l)

pushSingleStorePath ::
  (MonadMask m, MonadIO m) =>
  -- | details for pushing to cache
  PushParams m r ->
  -- | store path
  Store.StorePath ->
  -- | r is determined by the 'PushStrategy'
  m r
pushSingleStorePath cache storePath = retryAll $ \retrystatus -> do
  let name = pushParamsName cache
      strategy = pushParamsStrategy cache storePath
  -- Check if narinfo already exists
  res <-
    liftIO $
      (`runClientM` pushParamsClientEnv cache) $
        API.narinfoHead
          cachixClient
          (getCacheAuthToken (pushParamsSecret cache))
          name
          (NarInfoHash.NarInfoHash (Store.getStorePathHash storePath))
  case res of
    Right NoContent -> onAlreadyPresent strategy -- we're done as store path is already in the cache
    Left err
      | isErr err status404 -> uploadStorePath cache storePath retrystatus
      | isErr err status401 -> on401 strategy err
      | otherwise -> onError strategy err

getCacheAuthToken :: PushSecret -> Token
getCacheAuthToken (PushToken token) = token
getCacheAuthToken (PushSigningKey token _) = token

uploadStorePath ::
  (MonadIO m) =>
  -- | details for pushing to cache
  PushParams m r ->
  Store.StorePath ->
  RetryStatus ->
  -- | r is determined by the 'PushStrategy'
  m r
uploadStorePath cache storePath retrystatus = do
  let store = pushParamsStore cache
      storePathText = Store.getPath storePath
  let (storeHash, storeSuffix) = splitStorePath $ toS storePathText
      cacheName = pushParamsName cache
      authToken = getCacheAuthToken (pushParamsSecret cache)
      clientEnv = pushParamsClientEnv cache
      strategy = pushParamsStrategy cache storePath
      withCompressor = case compressionMethod strategy of
        BinaryCache.XZ -> defaultWithXzipCompressorWithLevel (compressionLevel strategy)
        BinaryCache.ZSTD -> defaultWithZstdCompressorWithLevel (compressionLevel strategy)
      cacheClientEnv =
        clientEnv
          { baseUrl = (baseUrl clientEnv) {baseUrlHost = toS cacheName <> "." <> baseUrlHost (baseUrl clientEnv)}
          }

  narSizeRef <- liftIO $ newIORef 0
  fileSizeRef <- liftIO $ newIORef 0
  narHashRef <- liftIO $ newIORef ("" :: ByteString)
  fileHashRef <- liftIO $ newIORef ("" :: ByteString)

  normalized <- liftIO $ Store.followLinksToStorePath store $ toS storePathText
  pathinfo <- liftIO $ escalateAs FatalError =<< Store.queryPathInfo store (toS normalized)
  let storePathSize = Store.narSize pathinfo
  onAttempt strategy retrystatus storePathSize

  withCompressor $ \compressor -> liftIO $ do
    uploadResult <-
      runConduitRes $
        streamNarIO narEffectsIO (toS storePathText) Data.Conduit.yield
          .| passthroughSizeSink narSizeRef
          .| passthroughHashSink narHashRef
          .| compressor
          .| passthroughSizeSink fileSizeRef
          .| passthroughHashSinkB16 fileHashRef
          .| Push.S3.streamUpload cacheClientEnv authToken cacheName (compressionMethod strategy)

    case uploadResult of
      Left err -> throwIO err
      Right (narId, uploadId, parts) -> do
        narSize <- readIORef narSizeRef
        narHash <- ("sha256:" <>) . System.Nix.Base32.encode <$> readIORef narHashRef
        nixNarHash <- escalateAs (FatalError . toS) $ Store.base16to32 $ Store.narHash pathinfo
        when (narHash /= nixNarHash) $ throwM $ NarHashMismatch $ toS storePathText <> ": Nar hash mismatch between nix-store --dump (" <> narHash <> ") and nix db (" <> nixNarHash <> ").\nYou can repair db metadata by running as root: $ nix-store --verify --repair --check-contents"
        fileHash <- readIORef fileHashRef
        fileSize <- readIORef fileSizeRef
        let deriver =
              if omitDeriver strategy
                then Nothing
                else Store.getStorePathBaseName . Store.StorePath <$> Store.deriver pathinfo
            references = sort $ Store.references pathinfo
        let fp = fingerprint storePathText narHash narSize references
            sig = case pushParamsSecret cache of
              PushToken _ -> Nothing
              PushSigningKey _ signKey -> Just $ (toS :: ByteString -> Text) $ convertToBase Base64 $ unSignature $ dsign (signingSecretKey signKey) fp
            nic =
              Api.NarInfoCreate
                { Api.cStoreHash = storeHash,
                  Api.cStoreSuffix = storeSuffix,
                  Api.cNarHash = narHash,
                  Api.cNarSize = narSize,
                  Api.cFileSize = fileSize,
                  Api.cFileHash = toS fileHash,
                  Api.cReferences = Store.getStorePathBaseName . Store.StorePath <$> references,
                  Api.cDeriver = fromMaybe "unknown-deriver" deriver,
                  Api.cSig = sig
                }
        escalate $ Api.isNarInfoCreateValid nic

        -- Complete the multipart upload and upload the narinfo
        let completeMultipartUploadRequest =
              API.completeNarUpload cachixClient authToken cacheName narId uploadId $
                Multipart.CompletedMultipartUpload
                  { Multipart.parts = parts,
                    Multipart.narInfoCreate = nic
                  }
        void $ withClientM completeMultipartUploadRequest cacheClientEnv escalate

  onDone strategy

-- | Push an entire closure
--
-- Note: 'onAlreadyPresent' will be called less often in the future.
pushClosure ::
  (MonadIO m, MonadMask m) =>
  -- | Traverse paths, responsible for bounding parallel processing of paths
  --
  -- For example: @'mapConcurrentlyBounded' 4@
  (forall a b. (a -> m b) -> [a] -> m [b]) ->
  PushParams m r ->
  -- | Initial store paths
  [Store.StorePath] ->
  -- | Every @r@ per store path of the entire closure of store paths
  m [r]
pushClosure traversal pushParams inputStorePaths = do
  missingPaths <- getMissingPathsForClosure pushParams inputStorePaths
  traversal (\path -> retryAll $ \retrystatus -> uploadStorePath pushParams path retrystatus) missingPaths

getMissingPathsForClosure :: (MonadIO m, MonadMask m) => PushParams m r -> [Store.StorePath] -> m [Store.StorePath]
getMissingPathsForClosure pushParams inputPaths = do
  let store = pushParamsStore pushParams
      clientEnv = pushParamsClientEnv pushParams
  -- Get the transitive closure of dependencies
  paths <- liftIO $ Store.computeClosure store inputPaths
  let pathsAndHashes = (\path -> (,) (Store.getStorePathHash path) path) <$> paths
  -- Check what store paths are missing
  missingHashesList <-
    retryAll $ \_ ->
      escalate
        =<< liftIO
          ( (`runClientM` clientEnv) $
              API.narinfoBulk
                cachixClient
                (getCacheAuthToken (pushParamsSecret pushParams))
                (pushParamsName pushParams)
                (map fst pathsAndHashes)
          )
  let missingHashes = Set.fromList missingHashesList
  return $ map snd $ filter (\(hash_, _path) -> Set.member hash_ missingHashes) pathsAndHashes

-- TODO: move to a separate module specific to cli

-- | Find auth token or signing key in the 'Config' or environment variable
findPushSecret ::
  Config.Config ->
  -- | Cache name
  Text ->
  -- | Secret key or exception
  IO PushSecret
findPushSecret config name = do
  maybeSigningKeyEnv <- toS <<$>> lookupEnv "CACHIX_SIGNING_KEY"
  maybeAuthToken <- Config.getAuthTokenMaybe config
  let maybeSigningKeyConfig = Config.secretKey <$> head (getBinaryCache config)
  case maybeSigningKeyEnv <|> maybeSigningKeyConfig of
    Just signingKey -> escalateAs FatalError $ PushSigningKey (fromMaybe (Token "") maybeAuthToken) <$> parseSigningKeyLenient signingKey
    Nothing -> case maybeAuthToken of
      Just authToken -> return $ PushToken authToken
      Nothing -> throwIO $ NoSigningKey msg
  where
    -- we reverse list of caches to prioritize keys added as last
    getBinaryCache c =
      reverse $
        filter (\bc -> Config.name bc == name) (Config.binaryCaches c)
    msg :: Text
    msg =
      [iTrim|
Neither auth token nor signing key are present.

They are looked up via $CACHIX_AUTH_TOKEN and $CACHIX_SIGNING_KEY,
and if missing also looked up from ~/.config/cachix/cachix.dhall

Read https://mycache.cachix.org for instructions how to push to your binary cache.
    |]

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
