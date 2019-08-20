module Main (main) where

import Cachix.Api (swaggerDoc)
import Data.Aeson (encode)

main :: IO ()
main = print $ encode swaggerDoc
