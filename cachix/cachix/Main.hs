module Main (main) where

import qualified Cachix.Client as CC
import Cachix.Client.CNix (handleCppExceptions)
import Cachix.Client.Exception (CachixException)
import Control.Exception.Safe (Handler (..), catches, displayException)
import GHC.IO.Encoding
import System.Exit (exitFailure)
import System.IO

main :: IO ()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  handleExceptions CC.main

handleExceptions :: IO () -> IO ()
handleExceptions f = f `catches` [Handler handleCachixExceptions, Handler handleCppExceptions]

handleCachixExceptions :: CachixException -> IO ()
handleCachixExceptions e = do
  hPutStrLn stderr ""
  hPutStr stderr (displayException e)
  hFlush stderr
  exitFailure
