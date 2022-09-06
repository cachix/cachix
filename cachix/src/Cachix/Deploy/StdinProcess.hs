module Cachix.Deploy.StdinProcess where

import Protolude hiding (stdin)
import System.IO (hClose)
import System.Process
import Prelude (String)

-- | Spawn a process with only stdin as an input
spawnProcess :: FilePath -> [String] -> String -> IO ()
spawnProcess cmd args input = do
  (Just stdin, _, _, _) <-
    createProcess
      (proc cmd args)
        { std_in = CreatePipe,
          -- When launching cachix-deployment we need to make sure it's not killed when the main process is killed
          create_group = True,
          -- Same, but with posix api
          new_session = True
        }
  hPutStr stdin input
  hClose stdin
