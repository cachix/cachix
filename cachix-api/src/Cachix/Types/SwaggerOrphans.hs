{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cachix.Types.SwaggerOrphans
  () where

import Data.Proxy
import Data.Swagger (ToParamSchema(..))
import Servant.API
import Servant.Swagger
import Servant.Streaming
import Servant.Auth.Swagger ()
import Web.Cookie           (SetCookie)

#if MIN_VERSION_swagger2(2,3,1)
#else
instance ToParamSchema SetCookie where
  toParamSchema _ = mempty
#endif

-- https://github.com/plow-technologies/servant-streaming/blob/master/servant-streaming-docs/src/Servant/Streaming/Docs/Internal.hs
-- TODO: these should define the body/response content
instance (HasSwagger api) => HasSwagger (StreamBodyMonad contentTypes m :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)

instance HasSwagger (StreamResponseGet contentTypes) where
  toSwagger _ = mempty -- TODO mkEndpointNoContent
