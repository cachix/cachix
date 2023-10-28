module Cachix.Client.Daemon.Push where

import qualified Cachix.API as API
import Cachix.Client.CNix (filterInvalidStorePath, followLinksToStorePath)
import Cachix.Client.Commands.Push hiding (pushStrategy)
import Cachix.Client.Daemon.Types (Daemon (..))
import Cachix.Client.Env (Env (..))
import Cachix.Client.OptionsParser as Client.OptionsParser
  ( PushOptions (..),
  )
import Cachix.Client.Push as Client.Push
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Conduit as C
import Control.Exception.Safe (throwM)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.Set as Set
import qualified Data.Text as T
import Hercules.CNix (StorePath)
import Hercules.CNix.Store (Store, storePathToPath, withStore)
import qualified Katip
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()

withPushParams :: Env -> PushOptions -> BinaryCache.BinaryCache -> PushSecret -> (PushParams Daemon () -> Daemon ()) -> Daemon ()
withPushParams env pushOpts binaryCache pushSecret m = do
  let authToken = getAuthTokenFromPushSecret pushSecret
  let compressionMethod = getCompressionMethod pushOpts binaryCache
  let cacheName = BinaryCache.name binaryCache

  withStore $ \store ->
    m
      PushParams
        { pushParamsName = cacheName,
          pushParamsSecret = pushSecret,
          pushParamsClientEnv = clientenv env,
          pushOnClosureAttempt = \full missing -> do
            let already = Set.toList $ Set.difference (Set.fromList full) (Set.fromList missing)
            b <- forM already $ \sp -> do
              p <- liftIO $ storePathToPath store sp
              return $ "Skipping " <> toS p
            Katip.logFM Katip.InfoS $ Katip.ls $ unlines b
            return missing,
          pushParamsStrategy = pushStrategy store authToken pushOpts cacheName compressionMethod,
          pushParamsStore = store
        }

pushStrategy :: Store -> Maybe Token -> PushOptions -> Text -> BinaryCache.CompressionMethod -> StorePath -> PushStrategy Daemon ()
pushStrategy store authToken opts name compressionMethod storePath = do
  PushStrategy
    { onAlreadyPresent = do
        sp <- liftIO $ storePathToPath store storePath
        Katip.logFM Katip.InfoS $ Katip.ls $ "Skipping " <> (toS sp :: Text),
      on401 = liftIO . handleCacheResponse name authToken,
      onError = throwM,
      onAttempt = \_ _ -> do
        sp <- liftIO $ storePathToPath store storePath
        Katip.logFM Katip.InfoS $ Katip.ls $ "Pushing " <> (toS sp :: Text),
      onUncompressedNARStream = \_ _ -> C.awaitForever C.yield,
      onDone = liftIO $ do
        sp <- storePathToPath store storePath
        putLText $ "Pushed " <> toS sp,
      Client.Push.compressionMethod = compressionMethod,
      Client.Push.compressionLevel = Client.OptionsParser.compressionLevel opts,
      Client.Push.omitDeriver = Client.OptionsParser.omitDeriver opts
    }

getBinaryCache :: Env -> Maybe Token -> BinaryCacheName -> IO BinaryCache.BinaryCache
getBinaryCache env authToken name = do
  -- Self-signed caches might not have a token, which is why this code is so weird.
  -- In practice, public self-signed caches don't need one and private ones always need a token.
  let token = fromMaybe (Token "") authToken
  res <- retryHttp $ (`runClientM` clientenv env) $ API.getCache cachixClient token name
  case res of
    Left err -> handleCacheResponse name authToken err
    Right binaryCache -> pure binaryCache

getCompressionMethod :: PushOptions -> BinaryCache.BinaryCache -> BinaryCache.CompressionMethod
getCompressionMethod opts binaryCache =
  fromMaybe BinaryCache.ZSTD $
    Client.OptionsParser.compressionMethod opts
      <|> Just (BinaryCache.preferredCompressionMethod binaryCache)

normalizeStorePath :: (MonadIO m) => Store -> FilePath -> m (Maybe StorePath)
normalizeStorePath store fp =
  liftIO $ runMaybeT $ do
    storePath <- MaybeT $ followLinksToStorePath store (encodeUtf8 $ T.pack fp)
    MaybeT $ filterInvalidStorePath store storePath
