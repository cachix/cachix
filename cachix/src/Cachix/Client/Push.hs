{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
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
import Cachix.Client.HumanSize (humanSize)
import qualified Cachix.Client.Push.S3 as Push.S3
import Cachix.Client.Retry (retryAll)
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Conduit
import qualified Data.Conduit.Lzma as Lzma (compress)
import qualified Data.Conduit.Zstd as Zstd (compress)
import Data.IORef
import qualified Data.Set as Set
import Data.String.Here
import qualified Data.Text as T
import Hercules.CNix (StorePath)
import qualified Hercules.CNix.Std.Set as Std.Set
import Hercules.CNix.Store (Store)
import qualified Hercules.CNix.Store as Store
import Network.HTTP.Types (status401, status404)
import Protolude hiding (toS)
import Protolude.Conv
import Servant.API
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()
import System.Console.AsciiProgress
import System.Console.Pretty
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice)
import qualified System.Nix.Base32
import System.Nix.Nar

data PushSecret
  = PushToken Token
  | PushSigningKey Token SigningKey

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
          (NarInfoHash.NarInfoHash (decodeUtf8With lenientDecode storeHash))
  case res of
    Right NoContent -> onAlreadyPresent strategy -- we're done as store path is already in the cache
    Left err
      | isErr err status404 -> uploadStorePath cache storePath retrystatus
      | isErr err status401 -> on401 strategy err
      | otherwise -> onError strategy err

getCacheAuthToken :: PushSecret -> Token
getCacheAuthToken (PushToken token) = token
getCacheAuthToken (PushSigningKey token _) = token

showUploadProgress store storePath retryStatus size = do
  let hSize = toS $ humanSize $ fromIntegral size
  path <- liftIO $ decodeUtf8With lenientDecode <$> Store.storePathToPath store storePath

  isTerminal <- liftIO $ hIsTerminalDevice stdout
  if isTerminal
    then do
      let bar = color Blue "[:bar] " <> toS path <> " (:percent of " <> hSize <> ")"
          barLength = T.length $ T.replace ":percent" "  0%" (T.replace "[:bar]" "" (toS bar))

      progressBar <-
        liftIO $
          newProgressBar
            def
              { pgTotal = fromIntegral size,
                -- https://github.com/yamadapc/haskell-ascii-progress/issues/24
                pgWidth = 20 + barLength,
                pgCompletedChar = '█',
                pgPendingChar = ' ',
                -- pgOnCompletion = Just $ color Green "✓ " <> toS path <> " (" <> hSize <> ")",
                pgOnCompletion = Nothing,
                pgFormat = bar
              }

      let onTick ratio = liftIO . tickN progressBar . ceiling . (max 1 ratio *) . fromIntegral . BS.length
      return (onTick, complete progressBar)
    else do
      -- we append newline instead of putStrLn due to https://github.com/haskell/text/issues/242
      putStr $ show retryStatus <> "Pushing " <> path <> " (" <> toS hSize <> ")\n"
      return (\_ -> const pass, pass)

uploadStorePath ::
  (MonadUnliftIO m) =>
  -- | details for pushing to cache
  PushParams m r ->
  StorePath ->
  RetryStatus ->
  -- | r is determined by the 'PushStrategy'
  m r
uploadStorePath cache storePath retrystatus = do
  let store = pushParamsStore cache
  -- TODO: storePathText is redundant. Use storePath directly.
  storePathText <- liftIO $ Store.storePathToPath store storePath
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

  -- This should be a noop because storePathText came from a StorePath
  normalized <- liftIO $ Store.followLinksToStorePath store $ toS storePathText
  pathinfo <- liftIO $ Store.queryPathInfo store normalized
  let storePathSize = Store.validPathInfoNarSize pathinfo
  onAttempt strategy retrystatus storePathSize

  (onUpload, finishProgress) <- showUploadProgress store storePath retrystatus storePathSize

  let modifiedOnUpload bs = do
        narSize <- readIORef narSizeRef
        fileSize <- readIORef fileSizeRef
        let ratio :: Double = fromIntegral narSize / fromIntegral fileSize
        onUpload ratio bs

  uploadResult <- withCompressor $ \compressor ->
    runConduitRes $
      streamNarIO narEffectsIO (toS storePathText) Data.Conduit.yield
        .| passthroughSizeSink narSizeRef
        .| passthroughHashSink narHashRef
        .| onUncompressedNARStream strategy retrystatus storePathSize
        .| compressor
        .| passthroughSizeSink fileSizeRef
        .| passthroughHashSinkB16 fileHashRef
        .| Push.S3.streamUpload cacheClientEnv authToken cacheName (compressionMethod strategy) modifiedOnUpload

  liftIO finishProgress

  case uploadResult of
    Left err -> throwIO err
    Right (narId, uploadId, parts) -> liftIO $ do
      narSize <- readIORef narSizeRef
      narHash <- ("sha256:" <>) . System.Nix.Base32.encode <$> readIORef narHashRef
      narHashNix <- Store.validPathInfoNarHash32 pathinfo
      when (narHash /= toS narHashNix) $ do
        print narHash
        print narHashNix
        throwM $ NarHashMismatch $ toS storePathText <> ": Nar hash mismatch between nix-store --dump and nix db. You can repair db metadata by running as root: $ nix-store --verify --repair --check-contents"
      fileHash <- readIORef fileHashRef
      fileSize <- readIORef fileSizeRef
      deriverPath <-
        if omitDeriver strategy
          then pure Nothing
          else Store.validPathInfoDeriver store pathinfo
      deriver <- for deriverPath Store.getStorePathBaseName
      referencesPathSet <- Store.validPathInfoReferences store pathinfo
      referencesPaths <- sort . fmap toS <$> for referencesPathSet (Store.storePathToPath store)
      references <- sort . fmap toS <$> for referencesPathSet Store.getStorePathBaseName
      let fp = fingerprint (decodeUtf8With lenientDecode storePathText) narHash narSize referencesPaths
          sig = case pushParamsSecret cache of
            PushToken _ -> Nothing
            PushSigningKey _ signKey -> Just $ toS $ B64.encode $ unSignature $ dsign (signingSecretKey signKey) fp
          nic =
            Api.NarInfoCreate
              { Api.cStoreHash = storeHash,
                Api.cStoreSuffix = storeSuffix,
                Api.cNarHash = narHash,
                Api.cNarSize = narSize,
                Api.cFileSize = fileSize,
                Api.cFileHash = toS fileHash,
                Api.cReferences = references,
                Api.cDeriver = maybe "unknown-deriver" (decodeUtf8With lenientDecode) deriver,
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
    retryAll $ \retrystatus -> uploadStorePath pushParams storePath retrystatus

getMissingPathsForClosure :: (MonadIO m, MonadMask m) => PushParams m r -> [StorePath] -> m ([StorePath], [StorePath])
getMissingPathsForClosure pushParams inputPaths = do
  let store = pushParamsStore pushParams
      clientEnv = pushParamsClientEnv pushParams
  -- Get the transitive closure of dependencies
  (paths :: [Store.StorePath]) <-
    liftIO $ do
      inputs <- Std.Set.new
      for_ inputPaths $ \path -> do
        Std.Set.insertFP inputs path
      closure <- Store.computeFSClosure store Store.defaultClosureParams inputs
      Std.Set.toListFP closure
  hashes <- for paths (liftIO . fmap (decodeUtf8With lenientDecode) . Store.getStorePathHash)
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
                hashes
          )
  let missingHashes = Set.fromList (encodeUtf8 <$> missingHashesList)
  pathsAndHashes <- liftIO $
    for paths $
      \path -> do
        hash_ <- Store.getStorePathHash path
        pure (hash_, path)
  let missing = map snd $ filter (\(hash_, _path) -> Set.member hash_ missingHashes) pathsAndHashes
  return (paths, missing)

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
