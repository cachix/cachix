module Cachix.Client.Terminal
  ( isCI,
    useInteractiveProgress,
  )
where

import Data.Char qualified as Char
import Protolude
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice)

isCI :: Maybe String -> Bool
isCI = maybe False $ \value -> fmap Char.toLower value `elem` ["true", "1"]

useInteractiveProgress :: Handle -> IO Bool
useInteractiveProgress handle = do
  isTerminal <- hIsTerminalDevice handle
  runningInCI <- isCI <$> lookupEnv "CI"
  return $ isTerminal && not runningInCI
