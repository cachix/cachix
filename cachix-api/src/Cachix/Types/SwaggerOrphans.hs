{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cachix.Types.SwaggerOrphans where

import Data.Conduit (ConduitT)
import Data.Swagger (ToSchema (..))
import Protolude

-- TODO: upstream to servant-conduit
instance ToSchema i => ToSchema (ConduitT i o m r) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy i)
-- TODO: Proxy o
