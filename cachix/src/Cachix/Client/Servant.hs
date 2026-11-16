{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cachix.Client.Servant
  ( isErr,
    cachixClient,
    listPins,
    deployClientV1,
    deployClientV2,
    withResponseTimeout,
  )
where

import Cachix.API qualified as API
import Cachix.API.Deploy.V1 qualified as API.Deploy.V1
import Cachix.API.Deploy.V2 qualified as API.Deploy.V2
import Cachix.API.Pins qualified as API.Pins
import Cachix.Types.ContentTypes ()
import Cachix.Types.Pin qualified as Pin
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (Status)
import Protolude
import Servant.API.Generic
import Servant.Auth.Client (Token)
import Servant.Client qualified
import Servant.Client.Streaming
import Servant.Conduit ()

-- | Override the timeout only for requests made by this client action.
withResponseTimeout :: HTTP.ResponseTimeout -> ClientM a -> ClientM a
withResponseTimeout responseTimeout = local $ \env ->
  env
    { makeClientRequest = \url request -> do
        httpRequest <- makeClientRequest env url request
        pure httpRequest {HTTP.responseTimeout = responseTimeout}
    }

isErr :: ClientError -> Status -> Bool
isErr (Servant.Client.FailureResponse _ resp) status
  | Servant.Client.responseStatusCode resp == status = True
isErr _ _ = False

cachixClient :: API.BinaryCacheAPI (AsClientT ClientM)
cachixClient = fromServant $ client (Proxy @API.API)

listPins :: Token -> Text -> ClientM [Pin.Pin]
listPins = client (Proxy @API.Pins.API)

deployClientV1 :: API.Deploy.V1.DeployAPI (AsClientT ClientM)
deployClientV1 = fromServant $ client (Proxy @API.Deploy.V1.API)

deployClientV2 :: API.Deploy.V2.DeployAPI (AsClientT ClientM)
deployClientV2 = fromServant $ client (Proxy @API.Deploy.V2.API)
