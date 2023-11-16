{-# LANGUAGE TemplateHaskell #-}

module Cachix.Client.Daemon.Status where

import Cachix.Client.Config.Orphans ()
import qualified Cachix.Client.Daemon.Protocol as Protocol
import qualified Cachix.Client.Daemon.Push as Push
import Cachix.Client.Daemon.ShutdownLatch (ShutdownLatch)
import Cachix.Client.Daemon.Types (Daemon, DaemonEnv (..))
import Cachix.Client.Env as Env
import Cachix.Client.OptionsParser (PushOptions)
import qualified Cachix.Client.OptionsParser as Options
import Cachix.Client.Push
import Cachix.Types.BinaryCache (BinaryCache, BinaryCacheName, CompressionMethod)
import qualified Cachix.Types.BinaryCache as BinaryCache
import Data.Aeson.TH
import Protolude
import System.Posix.Types (ProcessID)

data Configuration = Configuration
  { confPid :: Int32,
    confSocket :: FilePath,
    confNumWorkers :: Int,
    confBinaryCache :: BinaryCache,
    confCompression :: CompressionMethod
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''Configuration)

mkConfiguration :: Daemon Configuration
mkConfiguration = do
  DaemonEnv {..} <- ask
  return $
    Configuration
      { confPid = fromIntegral daemonPid,
        confSocket = daemonSocketPath,
        confNumWorkers = Options.numJobs daemonPushOptions,
        confBinaryCache = daemonBinaryCache,
        confCompression = Push.getCompressionMethod daemonPushOptions daemonBinaryCache
      }
