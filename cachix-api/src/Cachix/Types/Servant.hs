{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant specific additions that could be upstreamed
module Cachix.Types.Servant
  ( Get302,
    Post302,
    Head,
  )
where

import Protolude
import Servant.API

-- Location header as per https://github.com/haskell-servant/servant/issues/117#issuecomment-381398666
type Get302 (cts :: [Type]) (hs :: [Type]) = Verb 'GET 302 cts (Headers (Header "Location" Text ': hs) NoContent)

type Post302 (cts :: [Type]) (hs :: [Type]) = Verb 'POST 302 cts (Headers (Header "Location" Text ': hs) NoContent)

-- TODO: allow empty CT with HEAD
type Head = Verb 'HEAD 200 '[JSON] NoContent
