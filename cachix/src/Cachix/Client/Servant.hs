{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O0 #-} -- TODO https://github.com/haskell-servant/servant/issues/986

module Cachix.Client.Servant
  ( isErr
  , cachixClient
  , cachixBCClient
  , cachixBCStreamingClient
  , runAuthenticatedClient
  , Cachix.Client.Servant.ClientError
  ) where

import           Protolude

import qualified Cachix.Api as Api
import           Cachix.Api.Error
import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.Env as Env
import qualified Cachix.Client.Exception as Exception
import           Network.HTTP.Types (Status)
import           Servant.API.Generic
import           Servant.Auth             ()
import           Servant.Auth.Client      (Token)
import qualified Servant.Client
import           Servant.Client.Generic   (AsClientT)
import           Servant.Client.Streaming hiding (ClientError)
import           Servant.Conduit          ()

type ClientError =
#if !MIN_VERSION_servant_client(0,16,0)
  Servant.Client.ServantError
#else
  Servant.Client.ClientError
#endif

isErr :: ClientError -> Status -> Bool
#if MIN_VERSION_servant_client(0,16,0)
isErr (Servant.Client.FailureResponse _ resp) status
#else
isErr (Servant.Client.FailureResponse resp) status
#endif
  | Servant.Client.responseStatusCode resp == status = True
isErr _ _ = False

cachixClient :: Api.CachixAPI (AsClientT ClientM)
cachixClient = fromServant $ client Api.servantApi

cachixBCClient :: Text -> Api.BinaryCacheAPI (AsClientT ClientM)
cachixBCClient name = fromServant $ Api.cache cachixClient name

cachixBCStreamingClient :: Text -> Api.BinaryCacheStreamingAPI (AsClientT ClientM)
cachixBCStreamingClient name = fromServant $ client (Proxy :: Proxy Api.BinaryCachStreamingServantAPI) name

runAuthenticatedClient :: NFData a => Env.Env -> (Token -> ClientM a) -> IO a
runAuthenticatedClient env m = do
  config <- escalate $ maybeToEither (Exception.NoConfig
     "Start with visiting https://cachix.org and copying the token to $ cachix authtoken <token>") (Env.config env)
  escalate <=< (`runClientM` Env.clientenv env) $
    m (Config.authToken config)
