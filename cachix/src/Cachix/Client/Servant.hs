{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O0 #-}

-- TODO https://github.com/haskell-servant/servant/issues/986

module Cachix.Client.Servant
  ( isErr,
    cachixClient,
    cachixBCClient,
    cachixBCStreamingClient,
    runAuthenticatedClient,
  )
where

import qualified Cachix.Api as Api
import Cachix.Api.Error
import qualified Cachix.Client.Config as Config
import qualified Cachix.Client.Env as Env
import Network.HTTP.Types (Status)
import Protolude
import Servant.API.Generic
import Servant.Auth ()
import Servant.Auth.Client (Token)
import qualified Servant.Client
import Servant.Client.Generic (AsClientT)
import Servant.Client.Streaming
import Servant.Conduit ()

isErr :: ClientError -> Status -> Bool
isErr (Servant.Client.FailureResponse _ resp) status
  | Servant.Client.responseStatusCode resp == status = True
isErr _ _ = False

cachixClient :: Api.CachixAPI (AsClientT ClientM)
cachixClient = fromServant $ client Api.servantApi

cachixBCClient :: Text -> Api.BinaryCacheAPI (AsClientT ClientM)
cachixBCClient name = fromServant $ Api.cache cachixClient name

cachixBCStreamingClient :: Text -> Api.BinaryCacheStreamingAPI (AsClientT ClientM)
cachixBCStreamingClient name = fromServant $ client (Proxy :: Proxy Api.BinaryCachStreamingServantAPI) name

runAuthenticatedClient :: NFData a => Env.Env -> (Token -> ClientM a) -> IO a
runAuthenticatedClient env action = do
  cachixAuthToken <- Config.getAuthTokenOptional (Env.config env)
  escalate <=< (`runClientM` Env.clientenv env) $ action cachixAuthToken
