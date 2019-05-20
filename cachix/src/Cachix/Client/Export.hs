module Cachix.Client.Export where

import           Protolude

import qualified Cachix.Api as Api
import qualified Cachix.Client.Config as Config
import           Cachix.Client.Servant
import           Cachix.Client.Env
import qualified Cachix.Formats.CachixSigningKey as CachixSigningKey
import qualified Cachix.Formats.CachixPullToken as CachixPullToken
import           Data.Aeson
import           Data.List (find)
import           Data.Validation
import qualified Servant.Auth.Client

exportPushSecrets :: Env -> Config.Config -> [Text] -> IO (Validation (NonEmpty Text) [Encoding])
exportPushSecrets env cfg caches = do
  vs <- for caches $ exportPushSecret env cfg
  pure $ concat <$> sequenceA vs

exportPushSecret :: Env -> Config.Config -> Text -> IO (Validation (NonEmpty Text) [Encoding])
exportPushSecret env cfg cacheName = do
  let foundCache = validationNel
                    $ maybeToEither ("Cache " <> cacheName <> " not found in your local configuration")
                    $ find (\c -> Config.name c == cacheName) (Config.binaryCaches cfg)

  for foundCache $ \cache -> do
    cacheInfo <- runAuthenticatedClient env (Api.get (cachixBCClient cacheName))
    pure (
        toEncodingSigningKey cache
        : [ toEncodingPullKey cfg cacheName | not (Api.isPublic cacheInfo) ]
      )

exportPullSecrets :: Env -> Config.Config -> [Text] -> IO (Validation (NonEmpty Text) [Encoding])
exportPullSecrets env cfg caches = do
  vs <- for caches $ exportPullSecret env cfg
  pure $ concat <$> sequenceA vs

exportPullSecret :: Env -> Config.Config -> Text -> IO (Validation (NonEmpty Text) [Encoding])
exportPullSecret env cfg cacheName = do
  
  cacheInfo <- runAuthenticatedClient env (Api.get (cachixBCClient cacheName))

  if Api.isPublic cacheInfo
  then pure [] <$ hPutStr stderr ("Note: cache " <> cacheName <>
                                 " is public, requiring no secret to pull.\n")
  else pure $ pure [ toEncodingPullKey cfg cacheName ]

toEncodingSigningKey :: Config.BinaryCacheConfig -> Encoding
toEncodingSigningKey bc = toEncoding $ CachixSigningKey.CachixSigningKey
  { CachixSigningKey.cacheName = Config.name bc
  , CachixSigningKey.secretKey = Config.secretKey bc
  }

-- TODO: Support cache-specific tokens
toEncodingPullKey :: Config.Config -> Text -> Encoding
toEncodingPullKey cfg bc = toEncoding $ CachixPullToken.CachixPullToken
  { CachixPullToken.cacheName = bc
  , CachixPullToken.secretToken = toSL $ Servant.Auth.Client.getToken $ Config.authToken cfg
  }
