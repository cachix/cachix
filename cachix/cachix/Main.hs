module Main (main) where

import Control.Exception (handle, SomeException)
import System.IO
import System.Exit (exitFailure)
import GHC.IO.Encoding

import qualified Cachix.Client as CC


main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  handleExceptions CC.main

handleExceptions :: IO a -> IO a
handleExceptions = handle handler
  where
    handler :: SomeException -> IO a
    handler e = do
      hPutStrLn stderr ""
      hPrint stderr e
      exitFailure
