module Cachix.Deploy.StdinProcess where

import Protolude hiding (stdin)
import System.IO (hClose)
import System.Process
import Prelude (String, userError)

-- | Run a process with only stdin as an input
readProcess :: FilePath -> [String] -> String -> IO ()
readProcess cmd args input = do
  (Just stdin, _, _, ph) <- createProcess (proc cmd args) {std_in = CreatePipe}
  hPutStr stdin input
  hClose stdin
  exitcode <- waitForProcess ph
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure code -> ioError $ userError $ "Process " <> show cmd <> " failed with exit code " <> show code
