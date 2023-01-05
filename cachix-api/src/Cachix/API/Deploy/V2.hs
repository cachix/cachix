{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.API.Deploy.V2 where

import Cachix.API (CachixAuth)
import qualified Cachix.API.Deploy.V1 as API.V1
import qualified Cachix.Types.Deploy as Deploy
import qualified Cachix.Types.DeployResponse.V2 as DeployResponse.V2
import Protolude
import Servant.API
import Servant.API.Generic

data DeployAPI route = DeployAPI
  { activate :: route :- Activate,
    getDeployment :: route :- API.V1.GetDeployment
  }
  deriving (Generic)

type Activate =
  CachixAuth
    :> "deploy"
    :> "activate"
    :> ReqBody '[JSON] Deploy.Deploy
    :> Post '[JSON] DeployResponse.V2.DeployResponse
