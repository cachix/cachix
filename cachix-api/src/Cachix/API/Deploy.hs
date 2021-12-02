{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.API.Deploy where

import Cachix.API (CachixAuth)
import qualified Cachix.Types.ByteStringStreaming as ByteStringStreaming
import qualified Cachix.Types.Deploy as Deploy
import qualified Cachix.Types.DeployResponse as DeployResponse
import Conduit (ConduitT, ResourceT)
import qualified Data.UUID as UUID
import Protolude
import Servant.API
import Servant.API.Generic

data DeployAPI route = DeployAPI
  { activate ::
      route
        :- CachixAuth
        :> "deploy"
        :> "activate"
        :> ReqBody '[JSON] Deploy.Deploy
        :> Post '[JSON] DeployResponse.DeployResponse
  }
  deriving (Generic)

type API = "api" :> "v1" :> ToServantApi DeployAPI

api :: Proxy API
api = Proxy
