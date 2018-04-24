{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cachix.Client.Servant
  ( AsClient
  ) where

import Servant.Client
import Servant.Generic

-- TODO: servant-generic-client https://github.com/chpatrick/servant-generic/issues/10
data AsClient
type instance AsClient :- api = Client ClientM api
