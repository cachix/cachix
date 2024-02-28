module Cachix.Client.Daemon.Push where

import qualified Cachix.API as API
import Cachix.Client.Commands.Push hiding (pushStrategy)
import qualified Cachix.Client.Daemon.PushManager as PushManager
import Cachix.Client.Daemon.Types (Daemon, DaemonEnv (..), PushManager)
import Cachix.Client.Env (Env (..))
import Cachix.Client.OptionsParser as Options
  ( PushOptions (..),
  )
import Cachix.Client.Push as Push
import qualified Cachix.Client.Push.Options as Push.Options
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant
import Cachix.Types.BinaryCache (BinaryCacheName)
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Data.Set as Set
import Hercules.CNix.Store (withStore)
import Protolude hiding (toS)
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()

withPushParams :: (PushParams PushManager () -> Daemon b) -> Daemon b
withPushParams m = do
  DaemonEnv {..} <- ask
  let authToken = getAuthTokenFromPushSecret daemonPushSecret
      cacheName = BinaryCache.name daemonBinaryCache
      compressionMethod = getCompressionMethod daemonPushOptions daemonBinaryCache

  withStore $ \store ->
    m $ do
      let pushStrategy = PushManager.newPushStrategy store authToken daemonPushOptions cacheName compressionMethod

      PushParams
        { pushParamsName = cacheName,
          pushParamsSecret = daemonPushSecret,
          pushParamsClientEnv = clientenv daemonEnv,
          pushOnClosureAttempt = \full missing -> do
            let already = Set.toList $ Set.difference (Set.fromList full) (Set.fromList missing)
            mapM_ (onAlreadyPresent . pushStrategy) already
            return missing,
          pushParamsStrategy = pushStrategy,
          pushParamsStore = store
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

getCompressionMethod :: Options.PushOptions -> BinaryCache.BinaryCache -> BinaryCache.CompressionMethod
getCompressionMethod opts binaryCache =
  fromMaybe Push.Options.defaultCompressionMethod $
    Options.compressionMethod opts
      <|> Just (BinaryCache.preferredCompressionMethod binaryCache)
