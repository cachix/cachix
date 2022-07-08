{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -O0 #-}

-- TODO https://github.com/haskell-servant/servant/issues/986

module Cachix.Client.Servant
  ( isErr,
    cachixClient,
    deployClient,
  )
where

import qualified Cachix.API
import qualified Cachix.API.Deploy as Cachix.Deploy.API
import Cachix.Types.ContentTypes ()
import Network.HTTP.Types (Status)
import Protolude
import Servant.API.Generic
import Servant.Auth.Client ()
import qualified Servant.Client
import Servant.Client.Generic (AsClientT)
import Servant.Client.Streaming
import Servant.Conduit ()

isErr :: ClientError -> Status -> Bool
isErr (Servant.Client.FailureResponse _ resp) status
  | Servant.Client.responseStatusCode resp == status = True
isErr _ _ = False

cachixClient :: Cachix.API.BinaryCacheAPI (AsClientT ClientM)
cachixClient = fromServant $ client Cachix.API.api

deployClient :: Cachix.Deploy.API.DeployAPI (AsClientT ClientM)
deployClient = fromServant $ client Cachix.Deploy.API.api
