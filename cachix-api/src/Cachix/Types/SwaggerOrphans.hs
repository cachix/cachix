{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cachix.Types.SwaggerOrphans
  where

import Control.DeepSeq (NFData)
import Data.Proxy (Proxy(..))
import Data.Conduit (ConduitT)
import Data.Swagger (ToParamSchema(..), ToSchema(..))
import Servant.API  (NoContent)


-- TODO: upstream to servant-conduit
instance ToSchema i => ToSchema (ConduitT i o m r) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy i)
  -- TODO: Proxy o

-- https://github.com/haskell-servant/servant/pull/1090
instance NFData NoContent
