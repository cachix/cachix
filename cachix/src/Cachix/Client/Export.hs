module Cachix.Client.Export where

import           Protolude

import qualified Cachix.Api as Api
import qualified Cachix.Client.Config as Config
import           Cachix.Client.Servant
import           Cachix.Client.Env
import qualified Cachix.Formats.CachixSigningKey as CachixSigningKey
import qualified Cachix.Formats.CachixPullToken as CachixPullToken
import qualified Cachix.Formats.CachixPublicKey as CachixPublicKey
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
        : [ toEncodingPullToken cfg cacheName | not (Api.isPublic cacheInfo) ]
        ++ toEncodingPublicKey cacheInfo
      )

exportPullSecrets :: Env -> Config.Config -> [Text] -> IO (Validation (NonEmpty Text) [Encoding])
exportPullSecrets env cfg caches = do
  vs <- for caches $ exportPullSecret env cfg
  pure $ concat <$> sequenceA vs

exportPullSecret :: Env -> Config.Config -> Text -> IO (Validation (NonEmpty Text) [Encoding])
exportPullSecret env cfg cacheName = do

  cacheInfo <- runAuthenticatedClient env (Api.get (cachixBCClient cacheName))

  pure
    $ pure
    $ [ toEncodingPullToken cfg cacheName | not (Api.isPublic cacheInfo) ]
    ++ toEncodingPublicKey cacheInfo

toEncodingSigningKey :: Config.BinaryCacheConfig -> Encoding
toEncodingSigningKey bc = toEncoding $ CachixSigningKey.CachixSigningKey
  { CachixSigningKey.cacheName = Config.name bc
  , CachixSigningKey.secretKey = Config.secretKey bc
  }

-- TODO: Support cache-specific tokens
toEncodingPullToken :: Config.Config -> Text -> Encoding
toEncodingPullToken cfg bc = toEncoding $ CachixPullToken.CachixPullToken
  { CachixPullToken.cacheName = bc
  , CachixPullToken.secretToken = toSL $ Servant.Auth.Client.getToken $ Config.authToken cfg
  }

toEncodingPublicKey :: Api.BinaryCache -> [Encoding]
toEncodingPublicKey bc =
  map
      (\sk -> toEncoding $ CachixPublicKey.CachixPublicKey
        { CachixPublicKey.cacheName = Api.name bc
        , CachixPublicKey.publicKey = sk
        }
      )
    $ Api.publicSigningKeys bc
