{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cachix.Client.Servant
  ( isErr
  ) where

import Protolude
import Network.HTTP.Types (Status)
import Servant.Client

isErr :: ServantError -> Status -> Bool
isErr (FailureResponse resp) status | responseStatusCode resp == status = True
isErr _ _ = False
