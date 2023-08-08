module Cachix.Client.Daemon.Listen
  ( listen,
    getSocketPath,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Types (QueuedPushRequest (..))
import Control.Concurrent.STM.TBMQueue
import Control.Monad.Catch as E
import qualified Data.Aeson as Aeson
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.BS
import Protolude
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    getXdgDirectory,
  )
import qualified System.Environment as System
import System.FilePath ((</>))

listen :: TBMQueue QueuedPushRequest -> Socket.Socket -> IO ()
listen queue sock =
  forever $ E.handleAll logException $ do
    (conn, _peerAddr) <- Socket.accept sock
    bs <- Socket.BS.recv conn 4096

    -- TODO: ignore empty requests (means that the connection was closed)
    case Aeson.eitherDecodeStrict bs of
      Left err -> putErrText $ "Failed to decode push request: " <> show err
      Right pushRequest -> do
        -- Socket.BS.sendAll conn "OK"
        atomically $ writeTBMQueue queue $ QueuedPushRequest pushRequest conn
  where
    logException e = putErrText $ "Exception in daemon listen thread: " <> show e

mapSyncException :: (Exception e1, Exception e2, Safe.MonadCatch m) => m a -> (e1 -> e2) -> m a
mapSyncException a f = a `Safe.catch` (Safe.throwM . f)

getSocketPath :: IO FilePath
getSocketPath = do
  socketDir <- getSocketDir
  return $ socketDir </> "cachix-daemon.sock"

getSocketDir :: IO FilePath
getSocketDir = do
  xdgRuntimeDir <- getXdgRuntimeDir
  let socketDir = xdgRuntimeDir </> "cachix"
  createDirectoryIfMissing True socketDir
  return socketDir

-- On systems with systemd: /run/user/$UID
-- Otherwise, fall back to XDG_CACHE_HOME
getXdgRuntimeDir :: IO FilePath
getXdgRuntimeDir = do
  xdgRuntimeDir <- System.lookupEnv "XDG_RUNTIME_DIR"
  cacheFallback <- getXdgDirectory XdgCache ""
  return $ fromMaybe cacheFallback xdgRuntimeDir
