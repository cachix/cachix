module Cachix.Client.Version where

import Data.Version (showVersion)
import Paths_cachix (version)
import Protolude

cachixVersion :: Text
cachixVersion = "cachix " <> versionNumber

versionNumber :: Text
versionNumber = toS $ showVersion version
