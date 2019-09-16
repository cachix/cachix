{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cachix.Types.SwaggerOrphans
  where

import Protolude
import Data.Conduit (ConduitT)
import Data.Swagger (ToSchema(..))
import Servant.API  (NoContent)


-- TODO: upstream to servant-conduit
instance ToSchema i => ToSchema (ConduitT i o m r) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy i)
  -- TODO: Proxy o

-- https://github.com/haskell-servant/servant/pull/1090
#if !MIN_VERSION_servant_client(0,16,0)
instance NFData NoContent
#endif
