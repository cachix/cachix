{-# LANGUAGE CPP #-}
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

#if !MIN_VERSION_servant_client(0,16,0)
#define ClientError ServantError
#endif

isErr :: ClientError -> Status -> Bool
#if MIN_VERSION_servant_client(0,16,0)
isErr (FailureResponse _ resp) status
#else
isErr (FailureResponse resp) status
#endif
  | responseStatusCode resp == status = True
isErr _ _ = False
