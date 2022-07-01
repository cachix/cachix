module Main (main) where

import qualified Cachix.Client as CC
import Cachix.Client.Exception (CachixException)
import Control.Exception.Safe (displayException, handle)
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
      hFlush stderr
      exitFailure

deployment :: Command -> IO ()
deployment command =
  return ()

-- websocket client: separate module
-- agent registration should be different so we know where to send the deployment vs agent commands
-- input should be a queue of deployments
-- new websocket connection
-- TODO: https://superuser.com/questions/1333069/how-to-track-all-child-processes-spawned-by-a-systemctl-service
