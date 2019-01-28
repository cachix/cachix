module Cachix.Client.Exception (CachixException(..)) where

import Protolude

data CachixException
  = UnsupportedNixVersion Text
  | UserEnvNotSet Text
  | MustBeRoot Text
  | NixOSInstructions Text
  | AmbiguousInput Text
  | NoInput Text
  | NoConfig Text
  | NetRcParseError Text
  | NarStreamingError ExitCode Text
  | DeprecatedCommand Text
  deriving (Show, Typeable)

instance Exception CachixException
