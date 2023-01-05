{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Cachix.API.Deploy
  ( module Deploy.V2,
    API,
  )
where

import Cachix.API.Deploy.V2 as Deploy.V2
import Protolude
import Servant.API
import Servant.API.Generic

type API = "api" :> "v2" :> ToServantApi Deploy.V2.DeployAPI
