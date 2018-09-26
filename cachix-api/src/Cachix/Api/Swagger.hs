{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cachix.Api.Swagger
  () where

import Data.Proxy
import Data.Swagger (ToParamSchema(..))
import Servant.API
import Servant.Swagger
import Servant.Streaming
import Servant.Auth.Swagger ()
import Web.Cookie           (SetCookie)


-- TODO: https://github.com/haskell-servant/servant-auth/pull/42#issuecomment-381279499
instance ToParamSchema SetCookie where
  toParamSchema _ = mempty -- TODO: cookie instances for swagger

-- https://github.com/plow-technologies/servant-streaming/blob/master/servant-streaming-docs/src/Servant/Streaming/Docs/Internal.hs
-- TODO: these should define the body/response content
instance (HasSwagger api) => HasSwagger (StreamBodyMonad contentTypes m :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)

instance HasSwagger (StreamResponseGet contentTypes) where
  toSwagger _ = mempty -- TODO mkEndpointNoContent
