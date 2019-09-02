module Main (main) where

import qualified Cachix.Client as CC
import Cachix.Client.Exception (CachixException)
import Control.Exception (displayException, handle)
import GHC.IO.Encoding
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  handleExceptions CC.main

handleExceptions :: IO a -> IO a
handleExceptions = handle handler
  where
    handler :: CachixException -> IO a
    handler e = do
      hPutStrLn stderr ""
      hPutStr stderr (displayException e)
      exitFailure
