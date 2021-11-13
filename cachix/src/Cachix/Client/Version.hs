module Cachix.Client.Version where 

import Protolude 
import Data.Version (showVersion)
import Paths_cachix (version)

cachixVersion :: Text
cachixVersion = "cachix " <> versionNumber

versionNumber :: Text
versionNumber = toS $ showVersion version