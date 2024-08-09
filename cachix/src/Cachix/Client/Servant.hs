{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cachix.Client.Servant
  ( isErr,
    cachixClient,
    deployClientV1,
    deployClientV2,
  )
where

import Cachix.API qualified as API
import Cachix.API.Deploy.V1 qualified as API.Deploy.V1
import Cachix.API.Deploy.V2 qualified as API.Deploy.V2
import Cachix.Types.ContentTypes ()
import Network.HTTP.Types (Status)
import Protolude
import Servant.API.Generic
import Servant.Auth.Client ()
import Servant.Client qualified
import Servant.Client.Streaming
import Servant.Conduit ()

isErr :: ClientError -> Status -> Bool
isErr (Servant.Client.FailureResponse _ resp) status
  | Servant.Client.responseStatusCode resp == status = True
isErr _ _ = False

cachixClient :: API.BinaryCacheAPI (AsClientT ClientM)
cachixClient = fromServant $ client (Proxy @API.API)

deployClientV1 :: API.Deploy.V1.DeployAPI (AsClientT ClientM)
deployClientV1 = fromServant $ client (Proxy @API.Deploy.V1.API)

deployClientV2 :: API.Deploy.V2.DeployAPI (AsClientT ClientM)
deployClientV2 = fromServant $ client (Proxy @API.Deploy.V2.API)
