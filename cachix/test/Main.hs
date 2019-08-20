module Main where

import Protolude
import qualified Spec
import Test.Hspec.Runner

main :: IO ()
main = hspecWith config Spec.spec
  where
    config =
      defaultConfig
        { configColorMode = ColorAlways
          }
