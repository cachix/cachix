module Main (main) where

import Cachix.Api (swaggerDoc)
import Data.Aeson (encode)
import Protolude

main :: IO ()
main = print $ encode swaggerDoc
