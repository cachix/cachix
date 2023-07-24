{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.API.Deploy.V2 where

import Cachix.API (CachixAuth)
import qualified Cachix.Types.Deploy as Deploy
import qualified Cachix.Types.DeployResponse.V2 as DeployResponse.V2
import Protolude
import Servant.API

type API = "api" :> "v2" :> ToServantApi DeployAPI

data DeployAPI route = DeployAPI
  { activate :: route :- Activate
  }
  deriving (Generic)

type Activate =
  CachixAuth
    :> "deploy"
    :> "activate"
    :> ReqBody '[JSON] Deploy.Deploy
    :> Post '[JSON] DeployResponse.V2.DeployResponse
