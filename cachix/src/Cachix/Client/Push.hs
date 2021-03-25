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
import Cachix.Client.Retry (retryAll)
import Cachix.Client.Secrets
import Cachix.Client.Servant
import qualified Cachix.Types.ByteStringStreaming
import qualified Cachix.Types.NarInfoCreate as Api
import qualified Cachix.Types.NarInfoHash as NarInfoHash
import Control.Concurrent.Async (mapConcurrently)
import qualified Control.Concurrent.QSem as QSem
import Control.Exception.Safe (MonadMask, throwM)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Retry (RetryStatus)
import Crypto.Sign.Ed25519
import qualified Data.ByteString.Base64 as B64
import Data.Coerce (coerce)
import Data.Conduit
import Data.Conduit.Lzma (compress)
import Data.Conduit.Process hiding (env)
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
import System.Environment (lookupEnv)
import qualified System.Nix.Base32

data PushSecret
  = PushToken Token
  | PushSigningKey Token SigningKey

data PushParams m r
  = PushParams
      { pushParamsName :: Text,
        pushParamsSecret :: PushSecret,
        -- | how to report results, (some) errors, and do some things
        pushParamsStrategy :: StorePath -> PushStrategy m r,
        -- | cachix base url, connection manager, see 'Cachix.Client.URI.defaultCachixBaseUrl', 'Servant.Client.mkClientEnv'
        pushParamsClientEnv :: ClientEnv,
        pushParamsStore :: Store
      }

data PushStrategy m r
  = PushStrategy
      { -- | Called when a path is already in the cache.
        onAlreadyPresent :: m r,
        onAttempt :: RetryStatus -> Int64 -> m (),
        on401 :: m r,
        onError :: ClientError -> m r,
        onDone :: m r,
        withXzipCompressor :: forall a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a,
        omitDeriver :: Bool
      }

defaultWithXzipCompressor :: forall m a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
defaultWithXzipCompressor = ($ compress (Just 2))

defaultWithXzipCompressorWithLevel :: Int -> forall m a. (ConduitM ByteString ByteString (ResourceT IO) () -> m a) -> m a
defaultWithXzipCompressorWithLevel l = ($ compress (Just l))

pushSingleStorePath ::
  (MonadMask m, MonadIO m) =>
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
    liftIO $ (`runClientM` pushParamsClientEnv cache) $
      API.narinfoHead
        cachixClient
        (getCacheAuthToken (pushParamsSecret cache))
        name
        (NarInfoHash.NarInfoHash (decodeUtf8With lenientDecode storeHash))
  case res of
    Right NoContent -> onAlreadyPresent strategy -- we're done as store path is already in the cache
    Left err
      | isErr err status404 -> uploadStorePath cache storePath retrystatus
      | isErr err status401 -> on401 strategy
      | otherwise -> onError strategy err

getCacheAuthToken :: PushSecret -> Token
getCacheAuthToken (PushToken token) = token
getCacheAuthToken (PushSigningKey token _) = token

uploadStorePath ::
  (MonadMask m, MonadIO m) =>
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
      name = pushParamsName cache
      clientEnv = pushParamsClientEnv cache
      strategy = pushParamsStrategy cache storePath
  narSizeRef <- liftIO $ newIORef 0
  fileSizeRef <- liftIO $ newIORef 0
  narHashRef <- liftIO $ newIORef ("" :: ByteString)
  fileHashRef <- liftIO $ newIORef ("" :: ByteString)
  -- This should be a noop because storePathText came from a StorePath
  normalized <- liftIO $ Store.followLinksToStorePath store $ toS storePathText
  pathinfo <- liftIO $ Store.queryPathInfo store normalized
  -- stream store path as xz compressed nar file
  let cmd = proc "nix-store" ["--dump", toS storePathText]
      storePathSize :: Int64
      storePathSize = Store.validPathInfoNarSize pathinfo
  onAttempt strategy retrystatus storePathSize
  -- create_group makes subprocess ignore signals such as ctrl-c that we handle in haskell main thread
  -- see https://github.com/haskell/process/issues/198
  (ClosedStream, stdoutStream, Inherited, cph) <- liftIO $ streamingProcess (cmd {create_group = True})
  withXzipCompressor strategy $ \xzCompressor -> do
    let stream' =
          stdoutStream
            .| passthroughSizeSink narSizeRef
            .| passthroughHashSink narHashRef
            .| xzCompressor
            .| passthroughSizeSink fileSizeRef
            .| passthroughHashSinkB16 fileHashRef
    let subdomain =
          -- TODO: multipart
          if (fromIntegral storePathSize / (1024 * 1024) :: Double) > 100
            then "api"
            else toS name
        newClientEnv =
          clientEnv
            { baseUrl = (baseUrl clientEnv) {baseUrlHost = subdomain <> "." <> baseUrlHost (baseUrl clientEnv)}
            }
    (_ :: NoContent) <-
      liftIO
        $ (`withClientM` newClientEnv)
          (API.createNar cachixClient (getCacheAuthToken (pushParamsSecret cache)) name (mapOutput coerce stream'))
        $ escalate
          >=> \NoContent -> do
            exitcode <- waitForStreamingProcess cph
            when (exitcode /= ExitSuccess) $ throwM $ NarStreamingError exitcode $ show cmd
            return NoContent
    (_ :: NoContent) <- liftIO $ do
      narSize <- readIORef narSizeRef
      narHash <- ("sha256:" <>) . System.Nix.Base32.encode <$> readIORef narHashRef
      narHashNix <- Store.validPathInfoNarHash32 pathinfo
      when (narHash /= toS narHashNix) $ throwM $ NarHashMismatch "Nar hash mismatch between nix-store --dump and nix db. You can repair db metadata by running as root: $ nix-store --verify --repair"
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
          (sig, authToken) = case pushParamsSecret cache of
            PushToken token -> (Nothing, token)
            PushSigningKey token signKey -> (Just $ toS $ B64.encode $ unSignature $ dsign (signingSecretKey signKey) fp, token)
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
      -- Upload narinfo with signature
      escalate <=< (`runClientM` clientEnv) $
        API.createNarinfo
          cachixClient
          authToken
          name
          (NarInfoHash.NarInfoHash storeHash)
          nic
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
  [StorePath] ->
  -- | Every @r@ per store path of the entire closure of store paths
  m [r]
pushClosure traversal pushParams inputStorePaths = do
  missingPaths <- getMissingPathsForClosure pushParams inputStorePaths
  traversal (\path -> retryAll $ \retrystatus -> uploadStorePath pushParams path retrystatus) missingPaths

getMissingPathsForClosure :: (MonadIO m, MonadMask m) => PushParams m r -> [StorePath] -> m [StorePath]
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
  pathsAndHashes <- liftIO $ for paths $ \path -> do
    hash_ <- Store.getStorePathHash path
    pure (hash_, path)
  return $ map snd $ filter (\(hash_, _path) -> Set.member hash_ missingHashes) pathsAndHashes

-- TODO: move to a separate module specific to cli

-- | Find auth token or signing key in the 'Config' or environment variable
findPushSecret ::
  Maybe Config.Config ->
  -- | Cache name
  Text ->
  -- | Secret key or exception
  IO PushSecret
findPushSecret config name = do
  maybeSigningKeyEnv <- toS <<$>> lookupEnv "CACHIX_SIGNING_KEY"
  maybeAuthToken <- Config.getAuthTokenMaybe config
  let maybeSigningKeyConfig = case config of
        Nothing -> Nothing
        Just cfg -> Config.secretKey <$> head (getBinaryCache cfg)
  case maybeSigningKeyEnv <|> maybeSigningKeyConfig of
    Just signingKey -> escalateAs FatalError $ PushSigningKey (fromMaybe (Token "") maybeAuthToken) <$> parseSigningKeyLenient signingKey
    Nothing -> case maybeAuthToken of
      Just authToken -> return $ PushToken authToken
      Nothing -> throwIO $ NoSigningKey msg
  where
    -- we reverse list of caches to prioritize keys added as last
    getBinaryCache c = filter (\bc -> Config.name bc == name) $ reverse $ Config.binaryCaches c
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
