module Cachix.Client.Daemon.Listen
  ( listen,
    getSocketPath,
    openSocket,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Types
import Control.Concurrent.STM.TBMQueue
import qualified Control.Exception.Safe as Safe
import qualified Data.Aeson as Aeson
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.BS
import Protolude
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    getXdgDirectory,
    removeFile,
  )
import qualified System.Environment as System
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

data ListenError
  = SocketError SomeException
  | DecodingError Text
  deriving stock (Show)

instance Exception ListenError where
  displayException = \case
    SocketError err -> "Failed to read from the daemon socket: " <> show err
    DecodingError err -> "Failed to decode request: " <> toS err

-- | The main daemon server loop.
--
-- The daemon listens for incoming push requests on the provided socket and queues them up for the worker thread.
listen :: TBMQueue QueuedPushRequest -> Socket.Socket -> IO Socket.Socket
listen queue sock = loop
  where
    loop = do
      result <- runExceptT $ readPushRequest sock

      case result of
        Right (ClientStop, clientConn) -> do
          putText "Received stop request, shutting down..."
          return clientConn
        Right (ClientPushRequest pushRequest, clientConn) -> do
          let queuedRequest = QueuedPushRequest pushRequest clientConn
          atomically $ writeTBMQueue queue queuedRequest
          loop
        Left (DecodingError err) -> do
          putErrText $ "Failed to decode request: " <> err
          loop
        Left err -> throwIO err

-- | Try to read and decode a push request.
readPushRequest :: Socket.Socket -> ExceptT ListenError IO (ClientMessage, Socket.Socket)
readPushRequest sock = do
  (bs, clientConn) <- readFromSocket `mapSyncException` SocketError
  decodeMessage clientConn bs
  where
    readFromSocket = liftIO $ do
      -- NOTE: this sets up a non-blocking socket to the client
      (conn, _peerAddr) <- Socket.accept sock
      bs <- Socket.BS.recv conn 4096
      return (bs, conn)

    decodeMessage conn bs =
      case Aeson.eitherDecodeStrict bs of
        Left err -> throwE $ DecodingError (toS err)
        Right pushRequest -> return (pushRequest, conn)

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

-- TODO: lock the socket
openSocket :: FilePath -> IO Socket.Socket
openSocket socketFilePath = do
  deleteSocketFileIfExists socketFilePath
  sock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
  Socket.bind sock $ Socket.SockAddrUnix socketFilePath
  -- setFileMode socketFilePath socketFileMode
  return sock
  where
    deleteSocketFileIfExists path =
      removeFile path `catch` handleDoesNotExist

    handleDoesNotExist e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e
