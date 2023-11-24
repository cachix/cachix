{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

{- This is a standalone module so it shouldn't depend on any CLI state like Env -}
module Cachix.Client.Push
  ( -- * Pushing a single path
    pushSingleStorePath,
    uploadStorePath,
    narinfoExists,
    streamStorePath,
    PushParams (..),
    PushSecret (..),
    getAuthTokenFromPushSecret,
    PushStrategy (..),
    defaultWithXzipCompressor,
    defaultWithXzipCompressorWithLevel,
    defaultWithZstdCompressor,
    defaultWithZstdCompressorWithLevel,

    -- * Pushing a closure of store paths
    pushClosure,
    getMissingPathsForClosure,
    mapConcurrentlyBounded,
  )
where

import qualified Cachix.API as API
import Cachix.API.Error
import Cachix.API.Signing (fingerprint, passthroughHashSink, passthroughHashSinkB16, passthroughSizeSink)
import Cachix.Client.Exception (CachixException (..))
import qualified Cachix.Client.Push.S3 as Push.S3
import Cachix.Client.Retry (retryAll, retryHttp)
import Cachix.Client.Secrets
import Cachix.Client.Servant
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Cachix.Types.MultipartUpload as Multipart
import qualified Cachix.Types.NarInfoCreate as Api
import qualified Cachix.Types.NarInfoHash as NarInfoHash
import Conduit (MonadUnliftIO)
import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Concurrent.QSem as QSem
import Control.Exception.Safe (MonadMask, throwM)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Retry (RetryStatus)
import Crypto.Sign.Ed25519
import qualified Data.ByteString.Base64 as B64
import Data.Conduit
import qualified Data.Conduit.Lzma as Lzma (compress)
import qualified Data.Conduit.Zstd as Zstd (compress)
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Text as T
import Hercules.CNix (StorePath)
import qualified Hercules.CNix.Std.Set as Std.Set
import Hercules.CNix.Store (Store)
import qualified Hercules.CNix.Store as Store
import Network.HTTP.Types (status401, status404)
import qualified Nix.NarInfo as NarInfo
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import qualified System.Nix.Base32
import System.Nix.Nar

-- | A secret for authenticating with a cache.
data PushSecret
  = -- | An auth token. Could be a personal, cache, or agent token.
    PushToken Token
  | -- | An auth token with a signing key for pushing to self-signed caches.
    PushSigningKey Token SigningKey

getAuthTokenFromPushSecret :: PushSecret -> Maybe Token
getAuthTokenFromPushSecret = \case
  PushToken token -> nullTokenToMaybe token
  PushSigningKey token _ -> nullTokenToMaybe token
  where
    nullTokenToMaybe (Token "") = Nothing
    nullTokenToMaybe (Token token) = Just $ Token token

-- | Parameters for pushing a closure of store paths, to be passed to 'pushClosure'.
-- This also contains the parameters for pushing a single path, in 'pushParamStrategy'.
data PushParams m r = PushParams
  { pushParamsName :: Text,
    pushParamsSecret :: PushSecret,
    -- | An action to run on each closure push attempt.
    -- It receives a list of all paths in the closure and a subset of those paths missing from the cache.
    -- It should return a list of paths to push to the cache.
    pushOnClosureAttempt ::
      -- All paths in the closure
      [StorePath] ->
      -- Paths that are missing in the cache
      [StorePath] ->
      -- Paths to push to the cache
      m [StorePath],
    -- | Hooks into different stages of the single path push process.
    pushParamsStrategy :: StorePath -> PushStrategy m r,
    -- | cachix base url, connection manager, see 'Cachix.Client.URI.defaultCachixBaseUrl', 'Servant.Client.mkClientEnv'
    pushParamsClientEnv :: ClientEnv,
    pushParamsStore :: Store
  }

-- | Parameters for pushing a single store path. See 'pushSingleStorePath'
data PushStrategy m r = PushStrategy
  { -- | An action to call when a path is already in the cache.
    onAlreadyPresent :: m r,
    -- | An action to run on each push attempt.
    onAttempt :: RetryStatus -> Int64 -> m (),
    -- | A conduit to inspect the NAR stream before compression.
    -- This is useful for tracking progress and computing hashes. The conduit must not modify the stream.
    onUncompressedNARStream :: RetryStatus -> Int64 -> ConduitT ByteString ByteString (ResourceT m) (),
    -- | An action to run when authentication fails.
    on401 :: ClientError -> m r,
    -- | An action to run when an error occurs.
    onError :: ClientError -> m r,
    -- | An action to run after the path is pushed successfully.
    onDone :: m r,
    compressionMethod :: BinaryCache.CompressionMethod,
    compressionLevel :: Int,
    omitDeriver :: Bool
  }

defaultWithXzipCompressor :: (MonadIO m) => (ConduitT ByteString ByteString m () -> b) -> b
defaultWithXzipCompressor = ($ Lzma.compress (Just 2))

defaultWithXzipCompressorWithLevel :: (MonadIO m) => Int -> (ConduitT ByteString ByteString m () -> b) -> b
defaultWithXzipCompressorWithLevel l = ($ Lzma.compress (Just l))

defaultWithZstdCompressor :: (MonadIO m) => (ConduitT ByteString ByteString m () -> b) -> b
defaultWithZstdCompressor = ($ Zstd.compress 8)

defaultWithZstdCompressorWithLevel :: (MonadIO m) => Int -> (ConduitT ByteString ByteString m () -> b) -> b
defaultWithZstdCompressorWithLevel l = ($ Zstd.compress l)

pushSingleStorePath ::
  (MonadMask m, MonadUnliftIO m) =>
  -- | details for pushing to cache
  PushParams m r ->
  -- | store path
  StorePath ->
  -- | r is determined by the 'PushStrategy'
  m r
pushSingleStorePath cache storePath = retryAll $ \retrystatus -> do
  storeHash <- liftIO $ Store.getStorePathHash storePath
  let strategy = pushParamsStrategy cache storePath
  res <- liftIO $ narinfoExists cache storeHash
  case res of
    Right NoContent -> onAlreadyPresent strategy -- we're done as store path is already in the cache
    Left err
      | isErr err status404 -> uploadStorePath cache storePath retrystatus
      | isErr err status401 -> on401 strategy err
      | otherwise -> onError strategy err

narinfoExists :: (MonadMask m, MonadUnliftIO m) => PushParams m r -> ByteString -> IO (Either ClientError NoContent)
narinfoExists cache storeHash = do
  let name = pushParamsName cache
  retryHttp $
    (`runClientM` pushParamsClientEnv cache) $
      API.narinfoHead
        cachixClient
        (getCacheAuthToken (pushParamsSecret cache))
        name
        (NarInfoHash.NarInfoHash (decodeUtf8With lenientDecode storeHash))

getCacheAuthToken :: PushSecret -> Token
getCacheAuthToken (PushToken token) = token
getCacheAuthToken (PushSigningKey token _) = token

uploadStorePath ::
  (MonadUnliftIO m) =>
  -- | details for pushing to cache
  PushParams m r ->
  StorePath ->
  RetryStatus ->
  -- | r is determined by the 'PushStrategy'
  m r
uploadStorePath pushParams storePath retrystatus = do
  let store = pushParamsStore pushParams
  -- TODO: storePathText is redundant. Use storePath directly.
  storePathText <- liftIO $ Store.storePathToPath store storePath
  let strategy = pushParamsStrategy pushParams storePath
      withCompressor = case compressionMethod strategy of
        BinaryCache.XZ -> defaultWithXzipCompressorWithLevel (compressionLevel strategy)
        BinaryCache.ZSTD -> defaultWithZstdCompressorWithLevel (compressionLevel strategy)

  narSizeRef <- liftIO $ newIORef 0
  fileSizeRef <- liftIO $ newIORef 0
  narHashRef <- liftIO $ newIORef ("" :: ByteString)
  fileHashRef <- liftIO $ newIORef ("" :: ByteString)

  -- This should be a noop because storePathText came from a StorePath
  normalized <- liftIO $ Store.followLinksToStorePath store $ toS storePathText
  pathinfo <- liftIO $ Store.queryPathInfo store normalized
  let storePathSize = Store.validPathInfoNarSize pathinfo
  onAttempt strategy retrystatus storePathSize

  withCompressor $ \compressor -> do
    let narStream =
          streamNarIO narEffectsIO (toS storePathText) Data.Conduit.yield
            .| passthroughSizeSink narSizeRef
            .| passthroughHashSink narHashRef
            .| onUncompressedNARStream strategy retrystatus storePathSize
            .| compressor
            .| passthroughSizeSink fileSizeRef
            .| passthroughHashSinkB16 fileHashRef
    let makeNarInfo = liftIO $ do
          narHash <- ("sha256:" <>) . System.Nix.Base32.encode <$> readIORef narHashRef
          narHashNix <- Store.validPathInfoNarHash32 pathinfo
          when (narHash /= toS narHashNix) $
            throwM $
              NarHashMismatch $
                toS storePathText <> ": Nar hash mismatch between nix-store --dump and nix db. You can repair db metadata by running as root: $ nix-store --verify --repair --check-contents"

          narSize <- readIORef narSizeRef
          fileHash <- readIORef fileHashRef
          fileSize <- readIORef fileSizeRef
          deriverPath <-
            if omitDeriver strategy
              then pure Nothing
              else Store.validPathInfoDeriver store pathinfo
          deriver <- for deriverPath Store.getStorePathBaseName
          referencesPathSet <- Store.validPathInfoReferences store pathinfo
          references <- sort . fmap toS <$> for referencesPathSet Store.getStorePathBaseName
          return $
            NarInfo.NarInfo
              { NarInfo.storePath = toS storePathText,
                NarInfo.url = "",
                NarInfo.compression = show $ compressionMethod strategy,
                NarInfo.fileHash = toS fileHash,
                NarInfo.fileSize = fileSize,
                NarInfo.narHash = toS narHash,
                NarInfo.narSize = narSize,
                NarInfo.references = Set.fromList references,
                NarInfo.deriver = decodeUtf8With lenientDecode <$> deriver,
                NarInfo.system = Nothing,
                NarInfo.sig = Nothing,
                NarInfo.ca = Nothing
              }
    streamStorePath pushParams narStream makeNarInfo

streamStorePath ::
  (MonadUnliftIO m) =>
  PushParams m r ->
  ConduitT () ByteString (ResourceT m) () ->
  m NarInfo.SimpleNarInfo ->
  m r
streamStorePath pushParams narStream narInfoM = do
  let cacheName = pushParamsName pushParams
      store = pushParamsStore pushParams
      authToken = getCacheAuthToken (pushParamsSecret pushParams)
      clientEnv = pushParamsClientEnv pushParams
      cacheClientEnv =
        clientEnv
          { baseUrl = (baseUrl clientEnv) {baseUrlHost = toS cacheName <> "." <> baseUrlHost (baseUrl clientEnv)}
          }
  storeDir <- Store.storeDir store
  dummyStorePath <- liftIO $ Store.parseStorePathBaseName "j4fwy5gi1rdlrlbk2c0vnbs7fmlm60a7-coreutils-9.1"
  uploadNarResult <-
    runConduitRes $
      narStream
        .| Push.S3.streamUpload cacheClientEnv authToken cacheName (compressionMethod (pushParamsStrategy pushParams dummyStorePath))
  narInfo <- narInfoM
  let storePath :: Text
      storePath = toS $ NarInfo.storePath narInfo
  storeStorePath <- liftIO $ Store.parseStorePath store $ toS storePath
  let strategy = pushParamsStrategy pushParams storeStorePath
  case uploadNarResult of
    Left err
      | isErr err status401 ->
          on401 strategy err
      | otherwise ->
          onError strategy err
    Right (narId, uploadId, parts) -> do
      let (storeHash, storeSuffix) = splitStorePath storePath
      let references = sort $ (fmap . fmap) toS Set.toList $ NarInfo.references narInfo
      let fpReferences = fmap (\x -> toS storeDir <> "/" <> x) references
      let fp = fingerprint storePath (NarInfo.narHash narInfo) (NarInfo.narSize narInfo) fpReferences
          sig = case pushParamsSecret pushParams of
            PushToken _ -> Nothing
            PushSigningKey _ signKey -> Just $ toS $ B64.encode $ unSignature $ dsign (signingSecretKey signKey) fp
          nic =
            Api.NarInfoCreate
              { Api.cStoreHash = storeHash,
                Api.cStoreSuffix = storeSuffix,
                Api.cNarHash = NarInfo.narHash narInfo,
                Api.cNarSize = NarInfo.narSize narInfo,
                Api.cFileSize = NarInfo.fileSize narInfo,
                Api.cFileHash = NarInfo.fileHash narInfo,
                Api.cReferences = references,
                Api.cDeriver = maybe "unknown-deriver" identity $ NarInfo.deriver narInfo,
                Api.cSig = sig
              }
      liftIO $ escalate $ Api.isNarInfoCreateValid nic

      -- Complete the multipart upload and upload the narinfo
      let completeMultipartUploadRequest =
            API.completeNarUpload cachixClient authToken cacheName narId uploadId $
              Multipart.CompletedMultipartUpload
                { Multipart.parts = parts,
                  Multipart.narInfoCreate = nic
                }
      liftIO $ void $ retryHttp $ withClientM completeMultipartUploadRequest cacheClientEnv escalate

      onDone strategy

-- | Push an entire closure
--
-- Note: 'onAlreadyPresent' will be called less often in the future.
pushClosure ::
  (MonadMask m, MonadUnliftIO m) =>
  -- | Traverse paths, responsible for bounding parallel processing of paths
  --
  -- For example: @'mapConcurrentlyBounded' 4@
  (forall a b. (a -> m b) -> [a] -> m [b]) ->
  PushParams m r ->
  -- | Initial store paths
  [StorePath] ->
  -- | Every @r@ per store path of the entire closure of store paths
  m [r]
pushClosure traversal pushParams inputStorePaths = do
  (allPaths, missingPaths) <- getMissingPathsForClosure pushParams inputStorePaths
  paths <- pushOnClosureAttempt pushParams allPaths missingPaths
  flip traversal paths $ \storePath ->
    retryAll $ uploadStorePath pushParams storePath

getMissingPathsForClosure :: (MonadIO m, MonadMask m) => PushParams m r -> [StorePath] -> m ([StorePath], [StorePath])
getMissingPathsForClosure pushParams inputPaths = do
  let store = pushParamsStore pushParams
      clientEnv = pushParamsClientEnv pushParams
  -- Get the transitive closure of dependencies
  paths <- liftIO $ do
    inputs <- Std.Set.new
    for_ inputPaths $ Std.Set.insertFP inputs
    closure <- Store.computeFSClosure store Store.defaultClosureParams inputs
    Std.Set.toListFP closure
  hashes <- liftIO $ for paths (fmap (decodeUtf8With lenientDecode) . Store.getStorePathHash)
  -- Check what store paths are missing
  missingHashesList <-
    retryHttp $
      liftIO $
        escalate
          =<< API.narinfoBulk
            cachixClient
            (getCacheAuthToken (pushParamsSecret pushParams))
            (pushParamsName pushParams)
            hashes
          `runClientM` clientEnv
  let missingHashes = Set.fromList (encodeUtf8 <$> missingHashesList)
  pathsAndHashes <- liftIO $
    for paths $ \path -> do
      hash_ <- Store.getStorePathHash path
      pure (hash_, path)
  let missing = map snd $ filter (\(hash_, _path) -> Set.member hash_ missingHashes) pathsAndHashes
  return (paths, missing)

mapConcurrentlyBounded :: (Traversable t) => Int -> (a -> IO b) -> t a -> IO (t b)
mapConcurrentlyBounded bound action items = do
  qs <- QSem.newQSem bound
  let wrapped x = bracket_ (QSem.waitQSem qs) (QSem.signalQSem qs) (action x)
  mapConcurrently wrapped items

-------------------
-- Private terms --
splitStorePath :: Text -> (Text, Text)
splitStorePath storePath =
  (T.take 32 (T.drop 11 storePath), T.drop 44 storePath)
