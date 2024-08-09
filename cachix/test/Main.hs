module Main where

import Protolude
import Spec qualified
import Test.Hspec.Runner

main :: IO ()
main = hspecWith config Spec.spec
  where
    config =
      defaultConfig
        { configColorMode = ColorAlways
        }
