{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- | Servant specific additions that could be upstreamed
module Cachix.Types.Servant
  ( Get302
  ) where

import Data.Text (Text)
import Servant.API

-- Location header as per https://github.com/haskell-servant/servant/issues/117#issuecomment-381398666
type Get302 (cts :: [*]) (hs :: [*]) = Verb 'GET 302 cts (Headers (Header "Location" Text ': hs) NoContent)
