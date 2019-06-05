{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cachix.Client.Servant
  ( isErr
  , Cachix.Client.Servant.ClientError
  ) where

import Protolude
import Network.HTTP.Types (Status)
import qualified Servant.Client

type ClientError =
#if !MIN_VERSION_servant_client(0,16,0)
  Servant.Client.ServantError
#else
  Servant.Client.ClientError
#endif

isErr :: ClientError -> Status -> Bool
#if MIN_VERSION_servant_client(0,16,0)
isErr (Servant.Client.FailureResponse _ resp) status
#else
isErr (Servant.Client.FailureResponse resp) status
#endif
  | Servant.Client.responseStatusCode resp == status = True
isErr _ _ = False
