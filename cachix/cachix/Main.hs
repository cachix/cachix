module Main (main) where

import System.IO

import qualified Cachix.Client as CC


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  CC.main
