module Cachix.Daemon.Push
  ( newPushParams,
    getBinaryCache,
    getCompressionMethod,
  )
where

import Cachix.API qualified as API
import Cachix.Client.Command.Push hiding (pushStrategy)
import Cachix.Client.Env (Env (..))
import Cachix.Client.OptionsParser as Client.OptionsParser
  ( PushOptions (..),
  )
import Cachix.Client.Push as Client.Push
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant
import Cachix.Daemon.PushManager qualified as PushManager
import Cachix.Daemon.Types (PushManager)
import Cachix.Types.BinaryCache (BinaryCache, BinaryCacheName)
import Cachix.Types.BinaryCache qualified as BinaryCache
import Data.Set qualified as Set
import Hercules.CNix.Store (Store)
import Protolude hiding (toS)
import Servant.Auth ()
import Servant.Auth.Client
import Servant.Client.Streaming
import Servant.Conduit ()

newPushParams ::
  Store ->
  ClientEnv ->
  BinaryCache ->
  PushSecret ->
  PushOptions ->
  PushParams PushManager ()
newPushParams store clientEnv binaryCache pushSecret pushOptions = do
  let authToken = getAuthTokenFromPushSecret pushSecret
      cacheName = BinaryCache.name binaryCache
      compressionMethod = getCompressionMethod pushOptions binaryCache
      pushStrategy = PushManager.newPushStrategy store authToken pushOptions cacheName compressionMethod

  PushParams
    { pushParamsName = cacheName,
      pushParamsSecret = pushSecret,
      pushParamsClientEnv = clientEnv,
      pushOnClosureAttempt = \full missing -> do
        let already = Set.toList $ Set.difference (Set.fromList full) (Set.fromList missing)
        mapM_ (onAlreadyPresent . pushStrategy) already
        return missing,
      pushParamsStrategy = pushStrategy,
      pushParamsStore = store
    }

getBinaryCache :: Env -> Maybe Token -> BinaryCacheName -> IO BinaryCache
getBinaryCache env authToken name = do
  -- Self-signed caches might not have a token, which is why this code is so weird.
  -- In practice, public self-signed caches don't need one and private ones always need a token.
  let token = fromMaybe (Token "") authToken
  res <- retryHttp $ (`runClientM` clientenv env) $ API.getCache cachixClient token name
  case res of
    Left err -> handleCacheResponse name authToken err
    Right binaryCache -> pure binaryCache

getCompressionMethod :: PushOptions -> BinaryCache -> BinaryCache.CompressionMethod
getCompressionMethod opts binaryCache =
  fromMaybe BinaryCache.ZSTD $
    Client.OptionsParser.compressionMethod opts
      <|> Just (BinaryCache.preferredCompressionMethod binaryCache)
