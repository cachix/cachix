module Cachix.Client.CNix where

import Hercules.CNix.Store (Store, StorePath, isValidPath, storePathToPath)
import Protolude
import System.Console.Pretty (Color (..), color)


filterInvalidStorePaths :: Store -> [StorePath] -> IO [Maybe StorePath]
filterInvalidStorePaths store storePaths =
  traverse (filterInvalidStorePath store) storePaths

filterInvalidStorePath :: Store -> StorePath -> IO (Maybe StorePath)
filterInvalidStorePath store storePath = do
  isValid <- isValidPath store storePath
  if isValid
  then return $ Just storePath
  else do 
    path <- storePathToPath store storePath
    hPutStrLn stderr $ color Yellow $ "Warning: " <> (decodeUtf8With lenientDecode $ path) <> " is not valid, skipping"
    return Nothing