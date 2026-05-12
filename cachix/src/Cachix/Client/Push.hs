{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
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
    Push.S3.UploadMultipartResult (..),
    UploadNarDetails (..),
    streamUploadNar,
    streamCopy,
    completeNarUpload,

    -- * Narinfo
    narinfoExists,
    newNarInfoCreate,
    queryNarInfoBulk,

    -- * Pushing a closure of store paths
    pushClosure,
    computeClosure,
    getMissingPathsForClosure,
    mapConcurrentlyBounded,
  )
where

import Cachix.API qualified as API
import Cachix.API.Error
import Cachix.API.Signing (fingerprint, passthroughHashSink, passthroughHashSinkB16, passthroughSizeSink)
import Cachix.Client.CNix (formatStorePathWarning, validateStorePath)
import Cachix.Client.Exception (CachixException (..))
import Cachix.Client.Push.S3 qualified as Push.S3
import Cachix.Client.Retry (retryAll, retryHttp)
import Cachix.Client.Secrets
import Cachix.Client.Servant
import Cachix.Types.BinaryCache qualified as BinaryCache
import Cachix.Types.MultipartUpload qualified as Multipart
import Cachix.Types.NarInfoCreate qualified as Api
import Cachix.Types.NarInfoHash qualified as NarInfoHash
import Cachix.Types.Realisation qualified as Realisation
import Conduit (MonadUnliftIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem qualified as QSem
import Control.Exception.Safe (MonadCatch, MonadMask, throwM)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Retry (RetryStatus)
import Crypto.Sign.Ed25519
import Data.ByteString.Base64 qualified as B64
import Data.Conduit
import Data.Conduit.ByteString (ChunkSize)
import Data.Conduit.Lzma qualified as Lzma (compress)
import Data.Conduit.Zstd qualified as Zstd (compress)
import Data.IORef
import Data.List.Extra (chunksOf)
import Data.Set qualified as Set
import Data.String.Here (iTrim)
import Data.Text qualified as T
import Network.HTTP.Types (status401, status404)
import Nix.C.Hash.Nix32 qualified as Nix32
import Nix.C.Store.Derivation qualified as NixDrv
import Nix.C.Store.PathInfo (ContentAddress (..), PathInfoJsonFormat (..))
import Nix.C.Store.PathInfo qualified as NixPathInfo
import Nix.C.Store.Realisation qualified as NixRealisation
import Nix.C.Unsafe.Store (Derivation, Store, StorePath)
import Nix.C.Unsafe.Store qualified as Store
import Nix.NarInfo qualified as NarInfo
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import System.Nix.Nar
import System.OsPath qualified as OsPath

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
    nullTokenToMaybe token = Just token

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
    -- | The compression method to use.
    compressionMethod :: BinaryCache.CompressionMethod,
    -- | The compression level to use.
    compressionLevel :: Int,
    -- | The chunk size to use.
    chunkSize :: ChunkSize,
    -- | The number of chunks to upload concurrently.
    numConcurrentChunks :: Int,
    -- | Whether to mit the deriver from the narinfo.
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
  storeHash <- liftIO $ encodeUtf8 . Nix32.encode <$> Store.storePathHash storePath
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

  storePathOS <- liftIO $ Store.storeRealPath store storePath
  path <- liftIO $ OsPath.decodeFS storePathOS

  -- Validate the path is still valid (may have been GC'd since queued)
  liftIO (validateStorePath store storePath) >>= \case
    Right _ -> pure ()
    Left err -> throwM $ InvalidStorePath $ formatStorePathWarning path err
  pathInfo <- newPathInfoFromStorePath store storePath
  let narSize = fromIntegral (piNarSize pathInfo)

  onAttempt strategy retrystatus narSize

  eresult <-
    runConduitRes $
      streamNarIO narEffectsIO path Data.Conduit.yield
        .| streamUploadNar pushParams storePath narSize retrystatus

  case eresult of
    Left e -> onError strategy e
    Right (uploadResult, uploadNarDetails) -> do
      nic <- newNarInfoCreate pushParams storePath pathInfo uploadNarDetails
      completeNarUpload pushParams uploadResult nic
      pushBuildTraces pushParams pathInfo
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
  Push.S3.UploadMultipartResult ->
  Api.NarInfoCreate ->
  m ()
completeNarUpload pushParams Push.S3.UploadMultipartResult {..} nic = do
  let cacheName = pushParamsName pushParams
      authToken = getCacheAuthToken (pushParamsSecret pushParams)
      clientEnv = pushParamsClientEnv pushParams

  -- Complete the multipart upload and upload the narinfo
  let completeMultipartUploadRequest =
        API.completeNarUpload cachixClient authToken cacheName urNarId urUploadId $
          Multipart.CompletedMultipartUpload
            { Multipart.parts = urParts,
              Multipart.narInfoCreate = nic
            }

  liftIO $ void $ retryHttp $ withClientM completeMultipartUploadRequest clientEnv escalate

-- | Push build traces for content-addressed paths.
-- This is a no-op for non-CA paths (when 'piCa' is Nothing) or when
-- the deriver is unavailable. Walks the derivation graph via the
-- C API: for each output of the deriver and each output of each
-- (statically-known) input derivation, query the local store for a
-- realisation and push the collected list in one batch.
pushBuildTraces ::
  (MonadUnliftIO m) =>
  PushParams n r ->
  PathInfo ->
  m ()
pushBuildTraces pushParams pathInfo =
  case (piCa pathInfo, piDeriver pathInfo) of
    (Just _, Just deriver) | deriver /= unknownDeriver -> liftIO $ do
      let store = pushParamsStore pushParams
          cacheName = pushParamsName pushParams
          authToken = getCacheAuthToken (pushParamsSecret pushParams)
          clientEnv = pushParamsClientEnv pushParams
      -- Build the full drv store path from store-dir + deriver basename.
      storeDirFP <- OsPath.decodeFS =<< Store.storeDir store
      let drvFullFP = storeDirFP <> "/" <> toS deriver
      drvFullOs <- OsPath.encodeFS drvFullFP
      eDrvSp <- try (Store.parseStorePath' store drvFullOs)
      case eDrvSp of
        Left (_ :: SomeException) -> pure () -- drv missing or unparseable
        Right drvSp -> do
          visitedDrvsRef <- newIORef Set.empty
          visitedIdsRef <- newIORef Set.empty
          buildTraces <- collectBuildTraces visitedDrvsRef visitedIdsRef store drvSp
          unless (null buildTraces) $ do
            let request = API.putBuildTraces cachixClient authToken cacheName buildTraces
            void $ retryHttp $ withClientM request clientEnv escalate
    _ -> pure ()

-- | Walk the derivation input graph collecting any locally-recorded
-- realisations. Each drv is parsed once (cycle detection).
collectBuildTraces ::
  IORef (Set FilePath) -> -- visited drv full paths
  IORef (Set ByteString) -> -- visited drv_output_ids
  Store ->
  StorePath ->
  IO [Realisation.Realisation]
collectBuildTraces visitedDrvsRef visitedIdsRef store drvSp = do
  drvFP <- OsPath.decodeFS =<< Store.storeRealPath store drvSp
  alreadySeen <- Set.member drvFP <$> readIORef visitedDrvsRef
  if alreadySeen
    then pure []
    else do
      modifyIORef' visitedDrvsRef (Set.insert drvFP)
      eDrv <- try (Store.drvFromStorePath store drvSp)
      case eDrv of
        Left (_ :: SomeException) -> pure []
        Right drv -> do
          outputs <- NixDrv.derivationOutputs store drv drvSp
          here <- fmap catMaybes $ forM outputs $ \(_outName, drvOutputId) -> do
            already <- Set.member drvOutputId <$> readIORef visitedIdsRef
            if already
              then pure Nothing
              else do
                modifyIORef' visitedIdsRef (Set.insert drvOutputId)
                NixRealisation.queryRealisation store drvOutputId >>= \case
                  Nothing -> pure Nothing
                  Just r -> Just <$> mkBuildTrace store drvOutputId r
          -- Recurse into static input derivations. Dynamic inputs are
          -- not surfaced by the C API, so we skip the input walk for
          -- those cases (the realisations of statically-resolvable
          -- inputs are still collected above for the current drv).
          hasDynamic <- NixDrv.derivationHasDynamicInputs drv
          deps <-
            if hasDynamic
              then pure []
              else do
                pairs <- NixDrv.derivationInputDrvOutputs store drv
                let uniqueDrvPaths = Set.toList $ Set.fromList $ fmap fst pairs
                fmap concat $ forM uniqueDrvPaths $ \drvPathBS -> do
                  inputOs <- OsPath.encodeFS (toS drvPathBS :: FilePath)
                  inputSpE <- try (Store.parseStorePath' store inputOs)
                  case inputSpE of
                    Left (_ :: SomeException) -> pure []
                    Right inputSp ->
                      collectBuildTraces visitedDrvsRef visitedIdsRef store inputSp
          pure (here ++ deps)
  where
    -- \| The C binding emits ids of the form @\<storeDir\>/\<drvName\>^\<outputName\>@.
    -- Split on the last @^@ then take the basename of the LHS to get @\<drvName\>@.
    mkBuildTrace ::
      Store ->
      ByteString ->
      NixRealisation.Realisation ->
      IO Realisation.Realisation
    mkBuildTrace s drvOutputId r = do
      outSp <- NixRealisation.realisationOutPath r
      outBaseFP <-
        OsPath.decodeFS . OsPath.takeFileName =<< Store.storeRealPath s outSp
      sigs <- NixRealisation.realisationSignatures r
      let decode = decodeUtf8With lenientDecode
          (drvFullPathT, outputNameT) =
            case T.breakOnEnd "^" (decode drvOutputId) of
              (left, right) | not (T.null left) -> (T.dropEnd 1 left, right)
              _ -> ("", "")
          drvNameT = T.takeWhileEnd (/= '/') drvFullPathT
      pure
        Realisation.Realisation
          { Realisation.key =
              Realisation.DrvOutput
                { Realisation.drvPath = drvNameT,
                  Realisation.outputName = outputNameT
                },
            Realisation.value =
              Realisation.UnkeyedRealisation
                { Realisation.outPath = T.pack outBaseFP,
                  Realisation.signatures = fmap decode sigs
                }
          }

unknownDeriver :: Text
unknownDeriver = "unknown-deriver"

-- | Render the C-API 'ContentAddress' back to its textual narinfo form
-- (e.g. @"text:sha256:..."@ or @"fixed:r:sha256:..."@).
caToText :: ContentAddress -> Text
caToText = \case
  ContentAddressText t -> t
  ContentAddressStructured method hash ->
    method <> ":" <> NixPathInfo.hashToSRI hash

data UploadNarDetails = UploadNarDetails
  { undNarSize :: Integer,
    undNarHash :: Text,
    undFileSize :: Integer,
    undFileHash :: Text
  }
  deriving stock (Eq, Show)

-- | A simplified type for ValidPathInfo.
-- Can be constructed either from an existing NarInfo, or by querying the Nix store.
-- Only includes the common fields between remote and local path infos.
data PathInfo = PathInfo
  { piPath :: FilePath,
    piNarHash :: Text,
    piNarSize :: Integer,
    piDeriver :: Maybe Text,
    piReferences :: Set FilePath,
    piCa :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Create a 'PathInfo' for a store path by querying the Nix store.
newPathInfoFromStorePath :: (MonadIO m) => Store -> StorePath -> m PathInfo
newPathInfoFromStorePath store storePath = liftIO $ do
  osPath <- Store.storeRealPath store storePath
  path <- OsPath.decodeFS osPath
  -- V3 returns basenames for references and deriver, which is what we want.
  info <- Store.queryPathInfoJson store storePath PathInfoJsonFormatV3

  let narHash = NixPathInfo.hashToNix32 (NixPathInfo.pathInfoNarHash info)
      narSize = fromIntegral $ NixPathInfo.pathInfoNarSize info
      ca = caToText <$> NixPathInfo.pathInfoCa info
      refPaths = fmap (toS :: Text -> FilePath) (NixPathInfo.pathInfoReferences info)
      deriver = NixPathInfo.pathInfoDeriver info

  return $
    PathInfo
      { piPath = path,
        piNarHash = narHash,
        piNarSize = narSize,
        piDeriver = deriver,
        piReferences = Set.fromList refPaths,
        piCa = ca
      }

-- | Create a 'PathInfo' for a store path from an existing NarInfo.
newPathInfoFromNarInfo :: (Applicative m) => NarInfo.SimpleNarInfo -> m PathInfo
newPathInfoFromNarInfo narInfo =
  pure $
    PathInfo
      { piPath = NarInfo.storePath narInfo,
        piNarHash = NarInfo.narHash narInfo,
        piNarSize = NarInfo.narSize narInfo,
        piDeriver = NarInfo.deriver narInfo,
        piReferences = NarInfo.references narInfo,
        piCa = NarInfo.ca narInfo
      }

-- | A conduit that compresses and streams a NAR to a cache.
streamUploadNar ::
  (MonadUnliftIO m) =>
  PushParams m r ->
  StorePath ->
  Int64 ->
  RetryStatus ->
  ConduitT ByteString Void (ResourceT m) (Either ClientError (Push.S3.UploadMultipartResult, UploadNarDetails))
streamUploadNar pushParams storePath storePathSize retrystatus = do
  let cacheName = pushParamsName pushParams
      authToken = getCacheAuthToken (pushParamsSecret pushParams)
      clientEnv = pushParamsClientEnv pushParams

  let strategy = pushParamsStrategy pushParams storePath
      withCompressor = case compressionMethod strategy of
        BinaryCache.XZ -> defaultWithXzipCompressorWithLevel (compressionLevel strategy)
        BinaryCache.ZSTD -> defaultWithZstdCompressorWithLevel (compressionLevel strategy)

  let uploadOptions =
        Push.S3.UploadMultipartOptions
          { Push.S3.numConcurrentChunks = numConcurrentChunks strategy,
            Push.S3.chunkSize = chunkSize strategy,
            Push.S3.compressionMethod = compressionMethod strategy
          }

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
      .| Push.S3.uploadMultipart clientEnv authToken cacheName uploadOptions

  for result $ \uploadResult -> liftIO $ do
    narSize <- readIORef narSizeRef
    narHash <- ("sha256:" <>) . Nix32.encode <$> readIORef narHashRef
    fileSize <- readIORef fileSizeRef
    fileHash <- readIORef fileHashRef

    let uploadNarDetails =
          UploadNarDetails
            { undNarSize = narSize,
              undNarHash = narHash,
              undFileSize = fileSize,
              undFileHash = toS fileHash
            }

    return (uploadResult, uploadNarDetails)

-- | A conduit that streams an existing NAR to a cache.
-- Used to copy NARs between caches, e.g. S3 -> Cachix.
streamCopy ::
  (MonadUnliftIO m) =>
  PushParams m r ->
  StorePath ->
  Int64 ->
  RetryStatus ->
  BinaryCache.CompressionMethod ->
  ConduitT ByteString Void (ResourceT m) (Either ClientError (Push.S3.UploadMultipartResult, UploadNarDetails))
streamCopy pushParams storePath claimedFileSize retrystatus compressionMethod = do
  let cacheName = pushParamsName pushParams
      authToken = getCacheAuthToken (pushParamsSecret pushParams)
      clientEnv = pushParamsClientEnv pushParams
      strategy = pushParamsStrategy pushParams storePath

  let uploadOptions =
        Push.S3.UploadMultipartOptions
          { Push.S3.numConcurrentChunks = numConcurrentChunks strategy,
            Push.S3.chunkSize = chunkSize strategy,
            -- TODO: why is this not part of the strategy?
            Push.S3.compressionMethod = compressionMethod
          }

  fileSizeRef <- liftIO $ newIORef 0
  fileHashRef <- liftIO $ newIORef ("" :: ByteString)

  result <-
    awaitForever Data.Conduit.yield
      .| onUncompressedNARStream strategy retrystatus claimedFileSize
      .| passthroughSizeSink fileSizeRef
      .| passthroughHashSinkB16 fileHashRef
      .| Push.S3.uploadMultipart clientEnv authToken cacheName uploadOptions

  for result $ \uploadResult -> liftIO $ do
    fileSize <- readIORef fileSizeRef
    fileHash <- readIORef fileHashRef

    let uploadNarDetails =
          UploadNarDetails
            { undNarSize = 0,
              undNarHash = "",
              undFileSize = fileSize,
              undFileHash = toS fileHash
            }

    return (uploadResult, uploadNarDetails)

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
  storeDirOS <- liftIO $ Store.storeDir store
  storeDir <- liftIO $ OsPath.decodeFS storeDirOS
  storePathOS <- liftIO $ Store.storeRealPath store storePath
  storePathText <- liftIO $ toS <$> OsPath.decodeFS storePathOS

  when (undNarHash /= piNarHash pathInfo) $
    throwM $
      NarHashMismatch
        [iTrim|
${storePathText}: the computed NAR hash doesn't match the hash returned by the Nix store.

Expected: ${undNarHash}
Got:      ${piNarHash pathInfo}

1. Try repairing the Nix store:

   sudo nix-store --verify --repair --check-contents

2. If the issue persists, report it to https://github.com/cachix/cachix/issues
        |]

  let deriver =
        fromMaybe unknownDeriver $
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
            Api.cDeriver = deriver,
            Api.cSig = sig,
            Api.cCa = piCa pathInfo
          }

  escalate $ Api.isNarInfoCreateValid nic

  return nic

getMissingPathsForClosure :: (MonadIO m, MonadMask m) => PushParams n r -> [StorePath] -> m ([StorePath], [StorePath])
getMissingPathsForClosure pushParams inputPaths = do
  let store = pushParamsStore pushParams
  paths <- computeClosure store inputPaths
  queryNarInfoBulk pushParams paths

-- | Get the transitive closure of dependencies
computeClosure :: (MonadIO m) => Store -> [StorePath] -> m [StorePath]
computeClosure store inputPaths =
  liftIO $ do
    closureLists <- mapM (Store.computeFSClosure store) inputPaths
    pure $ Set.toList $ Set.fromList $ concat closureLists

queryNarInfoBulk :: (MonadIO m, MonadMask m) => PushParams n r -> [StorePath] -> m ([StorePath], [StorePath])
queryNarInfoBulk pushParams paths = do
  let clientEnv = pushParamsClientEnv pushParams

  pathsAndHashes <- liftIO $
    for paths $ \path -> do
      hash' <- Nix32.encode <$> Store.storePathHash path
      pure (hash', path)
  let hashes = fmap fst pathsAndHashes
  let processBatch hashesChunk = retryHttp $ liftIO $ do
        result <-
          API.narinfoBulk
            cachixClient
            (getCacheAuthToken (pushParamsSecret pushParams))
            (pushParamsName pushParams)
            hashesChunk
            `runClientM` clientEnv
        escalate result
  -- 32 bytes of each hash for 1MB limit
  missingHashesList <- concat <$> mapM processBatch (chunksOf 32768 hashes)

  let missingHashes = Set.fromList missingHashesList
  let missing = map snd $ filter (\(hash', _path) -> Set.member hash' missingHashes) pathsAndHashes
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
