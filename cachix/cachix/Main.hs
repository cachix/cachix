module Main (main) where

import qualified Cachix.Client as CC
import Control.Exception (SomeException, fromException, handle)
import GHC.IO.Encoding
import System.Exit (exitFailure, exitWith)
import System.IO
import Prelude

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
    handler e | (Just ee) <- fromException e = exitWith ee
    handler e = do
      hPutStrLn stderr ""
      -- TODO: pretty print the record once fixed https://github.com/haskell-servant/servant/issues/807
      hPrint stderr e
      exitFailure
