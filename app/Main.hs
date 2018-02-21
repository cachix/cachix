module Main (main) where

import Data.Aeson (encode)

import Cachix.Api (swaggerDoc)

main :: IO ()
main = print $ encode swaggerDoc
