{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}

{- This is a standalone module so it shouldn't depend on any CLI state like Env -}
module Cachix.Client.Push
  ( -- * Pushing a single path
    pushSingleStorePath,
    uploadStorePath,

    -- * Push strategy and parameters
    PushParams (..),
    PushSecret (..),
    getAuthTokenFromPushSecret,
    PushStrategy (..),
    defaultWithXzipCompressor,
    defaultWithXzipCompressorWithLevel,
    defaultWithZstdCompressor,
    defaultWithZstdCompressorWithLevel,

    -- * Path info
    PathInfo (..),
    newPathInfoFromStorePath,
    newPathInfoFromNarInfo,

    -- * Streaming upload
    UploadResult (..),
    UploadNarDetails (..),
    streamUploadNar,
    streamCopy,
    completeNarUpload,

    -- * Narinfo
    narinfoExists,
    newNarInfoCreate,

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
import Control.Exception.Safe (MonadCatch, MonadMask, throwM)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Retry (RetryStatus)
import Crypto.Sign.Ed25519
import qualified Data.ByteString.Base64 as B64
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Lzma as Lzma (compress)
import qualified Data.Conduit.Zstd as Zstd (compress)
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.UUID (UUID)
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
pushSingleStorePath pushParams storePath = retryAll $ \retrystatus -> do
  storeHash <- liftIO $ Store.getStorePathHash storePath
  let strategy = pushParamsStrategy pushParams storePath
  res <- liftIO $ narinfoExists pushParams storeHash
  case res of
    Right NoContent -> onAlreadyPresent strategy -- we're done as store path is already in the cache
    Left err
      | isErr err status404 -> uploadStorePath pushParams storePath retrystatus
      | isErr err status401 -> on401 strategy err
      | otherwise -> onError strategy err

narinfoExists :: PushParams m r -> ByteString -> IO (Either ClientError NoContent)
narinfoExists pushParams storeHash = do
  let cacheName = pushParamsName pushParams
      authToken = getCacheAuthToken (pushParamsSecret pushParams)
  retryHttp $
    (`runClientM` pushParamsClientEnv pushParams) $
      API.narinfoHead
        cachixClient
        authToken
        cacheName
        (NarInfoHash.NarInfoHash (decodeUtf8With lenientDecode storeHash))

getCacheAuthToken :: PushSecret -> Token
getCacheAuthToken (PushToken token) = token
getCacheAuthToken (PushSigningKey token _) = token

uploadStorePath ::
  (MonadUnliftIO m, MonadCatch m) =>
  -- | details for pushing to cache
  PushParams m r ->
  StorePath ->
  RetryStatus ->
  -- | r is determined by the 'PushStrategy'
  m r
uploadStorePath pushParams storePath retrystatus = do
  let store = pushParamsStore pushParams
      strategy = pushParamsStrategy pushParams storePath

  -- TODO: storePathText is redundant. Use storePath directly.
  storePathText <- liftIO $ Store.storePathToPath store storePath

  -- This should be a noop because storePathText came from a StorePath
  normalized <- liftIO $ Store.followLinksToStorePath store storePathText
  pathInfo <- newPathInfoFromStorePath store normalized
  let narSize = fromIntegral (piNarSize pathInfo)

  onAttempt strategy retrystatus narSize

  eresult <-
    runConduitRes $
      streamNarIO narEffectsIO (toS storePathText) Data.Conduit.yield
        .| streamUploadNar pushParams storePath narSize retrystatus

  case eresult of
    Left e -> onError strategy e
    Right uploadResult -> do
      nic <- newNarInfoCreate pushParams storePath pathInfo (murNarDetails uploadResult)
      completeNarUpload pushParams uploadResult nic
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

completeNarUpload ::
  (MonadUnliftIO m) =>
  PushParams n r ->
  UploadResult ->
  Api.NarInfoCreate ->
  m ()
completeNarUpload pushParams UploadResult {..} nic = do
  let cacheName = pushParamsName pushParams
      authToken = getCacheAuthToken (pushParamsSecret pushParams)
      clientEnv = pushParamsClientEnv pushParams

  -- Complete the multipart upload and upload the narinfo
  let completeMultipartUploadRequest =
        API.completeNarUpload cachixClient authToken cacheName murNarId murUploadId $
          Multipart.CompletedMultipartUpload
            { Multipart.parts = murParts,
              Multipart.narInfoCreate = nic
            }

  liftIO $ void $ retryHttp $ withClientM completeMultipartUploadRequest clientEnv escalate

data UploadResult = UploadResult
  { murNarId :: UUID,
    murUploadId :: Text,
    murParts :: Maybe (NonEmpty Multipart.CompletedPart),
    murNarDetails :: UploadNarDetails
  }
  deriving stock (Eq, Show)

data UploadNarDetails = UploadNarDetails
  { undNarSize :: Integer,
    undNarHash :: Text,
    undFileSize :: Integer,
    undFileHash :: Text
  }
  deriving stock (Eq, Show)

data PathInfo = PathInfo
  { piPath :: FilePath,
    piNarHash :: Text,
    piNarSize :: Integer,
    piDeriver :: Maybe Text,
    piReferences :: Set FilePath
  }
  deriving stock (Eq, Show)

newPathInfoFromStorePath :: (MonadIO m) => Store -> StorePath -> m PathInfo
newPathInfoFromStorePath store storePath = liftIO $ do
  pathInfo <- Store.queryPathInfo store storePath

  path <- Store.storePathToPath store storePath
  narHash <- Store.validPathInfoNarHash32 pathInfo
  let narSize = fromIntegral $ Store.validPathInfoNarSize pathInfo
  deriver <-
    Store.validPathInfoDeriver store pathInfo
      >>= mapM Store.getStorePathBaseName
  references <-
    Store.validPathInfoReferences store pathInfo
      >>= mapM Store.getStorePathBaseName

  return $
    PathInfo
      { piPath = toS path,
        piNarHash = decodeUtf8 narHash,
        piNarSize = narSize,
        piDeriver = fmap toS deriver,
        piReferences = Set.fromList (fmap toS references)
      }

newPathInfoFromNarInfo :: (Applicative m) => NarInfo.SimpleNarInfo -> m PathInfo
newPathInfoFromNarInfo narInfo =
  pure $
    PathInfo
      { piPath = NarInfo.storePath narInfo,
        piNarHash = NarInfo.narHash narInfo,
        piNarSize = NarInfo.narSize narInfo,
        piDeriver = NarInfo.deriver narInfo,
        piReferences = NarInfo.references narInfo
      }

streamUploadNar ::
  (MonadUnliftIO m) =>
  PushParams m r ->
  StorePath ->
  Int64 ->
  RetryStatus ->
  ConduitT ByteString Void (ResourceT m) (Either ClientError UploadResult)
streamUploadNar pushParams storePath storePathSize retrystatus = do
  let cacheName = pushParamsName pushParams
      authToken = getCacheAuthToken (pushParamsSecret pushParams)
      clientEnv = pushParamsClientEnv pushParams

  let strategy = pushParamsStrategy pushParams storePath
      withCompressor = case compressionMethod strategy of
        BinaryCache.XZ -> defaultWithXzipCompressorWithLevel (compressionLevel strategy)
        BinaryCache.ZSTD -> defaultWithZstdCompressorWithLevel (compressionLevel strategy)

  narSizeRef <- liftIO $ newIORef 0
  fileSizeRef <- liftIO $ newIORef 0
  narHashRef <- liftIO $ newIORef ("" :: ByteString)
  fileHashRef <- liftIO $ newIORef ("" :: ByteString)

  result <- withCompressor $ \compressor ->
    awaitForever Data.Conduit.yield
      .| passthroughSizeSink narSizeRef
      .| passthroughHashSink narHashRef
      .| onUncompressedNARStream strategy retrystatus storePathSize
      .| compressor
      .| passthroughSizeSink fileSizeRef
      .| passthroughHashSinkB16 fileHashRef
      .| Push.S3.streamUpload clientEnv authToken cacheName (compressionMethod strategy)

  for result $ \(narId, uploadId, mparts) -> liftIO $ do
    narSize <- readIORef narSizeRef
    narHash <- ("sha256:" <>) . System.Nix.Base32.encode <$> readIORef narHashRef
    fileSize <- readIORef fileSizeRef
    fileHash <- readIORef fileHashRef

    let uploadNarDetails =
          UploadNarDetails
            { undNarSize = narSize,
              undNarHash = narHash,
              undFileSize = fileSize,
              undFileHash = toS fileHash
            }

    return $
      UploadResult
        { murNarId = narId,
          murUploadId = uploadId,
          murParts = mparts,
          murNarDetails = uploadNarDetails
        }

streamCopy ::
  (MonadUnliftIO m) =>
  PushParams m r ->
  StorePath ->
  Int64 ->
  RetryStatus ->
  BinaryCache.CompressionMethod ->
  ConduitT ByteString Void (ResourceT m) (Either ClientError UploadResult)
streamCopy pushParams storePath storePathSize retrystatus compressionMethod = do
  let cacheName = pushParamsName pushParams
      authToken = getCacheAuthToken (pushParamsSecret pushParams)
      clientEnv = pushParamsClientEnv pushParams
      strategy = pushParamsStrategy pushParams storePath

  fileSizeRef <- liftIO $ newIORef 0
  fileHashRef <- liftIO $ newIORef ("" :: ByteString)

  result <-
    awaitForever Data.Conduit.yield
      .| passthroughSizeSink fileSizeRef
      .| passthroughHashSinkB16 fileHashRef
      .| Push.S3.streamUpload clientEnv authToken cacheName compressionMethod

  for result $ \(narId, uploadId, mparts) -> liftIO $ do
    fileSize <- readIORef fileSizeRef
    fileHash <- readIORef fileHashRef

    let uploadNarDetails =
          UploadNarDetails
            { undNarSize = 0,
              undNarHash = "",
              undFileSize = fileSize,
              undFileHash = toS fileHash
            }

    return $
      UploadResult
        { murNarId = narId,
          murUploadId = uploadId,
          murParts = mparts,
          murNarDetails = uploadNarDetails
        }

newNarInfoCreate ::
  (MonadIO m, MonadCatch m) =>
  PushParams n r ->
  StorePath ->
  PathInfo ->
  UploadNarDetails ->
  m Api.NarInfoCreate
newNarInfoCreate pushParams storePath pathInfo UploadNarDetails {..} = do
  let store = pushParamsStore pushParams
  let strategy = pushParamsStrategy pushParams storePath
  storeDir <- Store.storeDir store
  storePathText <- liftIO $ toS <$> Store.storePathToPath store storePath

  -- TODO: show expected vs actual NAR hash
  when (undNarHash /= piNarHash pathInfo) $
    throwM $
      NarHashMismatch $
        toS storePathText <> ": Nar hash mismatch between nix-store --dump and nix db. You can repair db metadata by running as root: $ nix-store --verify --repair --check-contents"

  let deriver =
        if omitDeriver strategy
          then Nothing
          else piDeriver pathInfo

  let references = fmap toS $ Set.toList $ piReferences pathInfo
  let fpReferences = fmap (\fp -> toS storeDir <> "/" <> fp) references

  let (storeHash, storeSuffix) = splitStorePath storePathText
  let fp = fingerprint storePathText undNarHash undNarSize fpReferences
      sig = case pushParamsSecret pushParams of
        PushToken _ -> Nothing
        PushSigningKey _ signKey -> Just $ toS $ B64.encode $ unSignature $ dsign (signingSecretKey signKey) fp

  let nic =
        Api.NarInfoCreate
          { Api.cStoreHash = storeHash,
            Api.cStoreSuffix = storeSuffix,
            Api.cNarHash = undNarHash,
            Api.cNarSize = undNarSize,
            Api.cFileSize = undFileSize,
            Api.cFileHash = undFileHash,
            Api.cReferences = references,
            Api.cDeriver = fromMaybe "unknown-deriver" deriver,
            Api.cSig = sig
          }

  -- TODO: turn into newNarInfoCreate
  escalate $ Api.isNarInfoCreateValid nic

  return nic

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
