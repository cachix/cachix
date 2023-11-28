module Cachix.Client.Exception (CachixException (..)) where

import Protolude

data CachixException
  = UnsupportedNixVersion Text
  | UserEnvNotSet Text
  | MustBeRoot Text
  | NixOSInstructions Text
  | AmbiguousInput Text
  | NoSigningKey Text
  | NoInput Text
  | NoConfig Text
  | NetRcParseError Text
  | NarStreamingError ExitCode Text
  | NarHashMismatch Text
  | DeprecatedCommand Text
  | ArtifactNotFound Text
  | AccessDeniedBinaryCache Text
  | BinaryCacheNotFound Text
  | ImportUnsupportedHash Text
  deriving (Show, Typeable)

instance Exception CachixException where
  displayException (UnsupportedNixVersion s) = toS s
  displayException (UserEnvNotSet s) = toS s
  displayException (MustBeRoot s) = toS s
  displayException (NixOSInstructions s) = toS s
  displayException (AmbiguousInput s) = toS s
  displayException (NoInput s) = toS s
  displayException (NoConfig s) = toS s
  displayException (ArtifactNotFound s) = toS s
  displayException (NoSigningKey s) = toS s
  displayException (NetRcParseError s) = toS s
  displayException (NarStreamingError _ s) = toS s
  displayException (NarHashMismatch s) = toS s
  displayException (DeprecatedCommand s) = toS s
  displayException (AccessDeniedBinaryCache s) = toS s
  displayException (BinaryCacheNotFound s) = toS s
  displayException (ImportUnsupportedHash s) = toS s
