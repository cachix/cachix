{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cachix.Client.Servant
  ( isErr
  , discardNoContent
  ) where

import Protolude
import Network.HTTP.Types (Status)
import Servant.API (NoContent)
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

-- | Convert 'NoContent' to '()' in order to silence a needless warning.
discardNoContent :: Functor f => f NoContent -> f ()
discardNoContent = void