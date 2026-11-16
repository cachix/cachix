{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Client definition for the existing server pin listing endpoint.
module Cachix.API.Pins (API) where

import Cachix.API (CachixAuth)
import Cachix.Types.Pin (Pin)
import Protolude
import Servant.API

type API =
  "api" :> "v1" :> CachixAuth :> "cache" :> Capture "name" Text :> "pin" :> Get '[JSON] [Pin]
