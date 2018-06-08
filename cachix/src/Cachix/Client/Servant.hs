{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cachix.Client.Servant
  ( AsClient
  , isErr
  ) where

import Protolude
import Network.HTTP.Types (Status)
import Servant.Client
import Servant.Generic

-- TODO: servant-generic-client https://github.com/chpatrick/servant-generic/issues/10
data AsClient
type instance AsClient :- api = Client ClientM api

isErr :: ServantError -> Status -> Bool
isErr (FailureResponse resp) status | responseStatusCode resp == status = True
isErr _ _ = False
