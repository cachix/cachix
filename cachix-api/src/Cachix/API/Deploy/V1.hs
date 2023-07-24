{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.API.Deploy.V1 where

import Cachix.API (CachixAuth)
import qualified Cachix.Types.Deploy as Deploy
import qualified Cachix.Types.DeployResponse.V1 as DeployResponse.V1
import qualified Cachix.Types.Deployment as Deployment
import Data.UUID (UUID)
import Protolude
import Servant.API

type API = "api" :> "v1" :> ToServantApi DeployAPI

data DeployAPI route = DeployAPI
  { activate :: route :- Activate,
    getDeployment :: route :- GetDeployment
  }
  deriving (Generic)

type Activate =
  CachixAuth
    :> "deploy"
    :> "activate"
    :> ReqBody '[JSON] Deploy.Deploy
    :> Post '[JSON] DeployResponse.V1.DeployResponse

type GetDeployment =
  CachixAuth
    :> "deploy"
    :> "deployment"
    :> Capture "uuid" UUID
    :> Get '[JSON] Deployment.Deployment
