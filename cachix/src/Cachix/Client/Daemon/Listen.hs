module Cachix.Client.Daemon.Listen
  ( listen,
    serverBye,
    getSocketPath,
    openSocket,
    closeSocket,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Client.Daemon.Protocol as Protocol
import Control.Exception.Safe (catchAny)
import qualified Control.Exception.Safe as Safe
import qualified Data.Aeson as Aeson
import qualified Katip
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as Socket.BS
import qualified Network.Socket.ByteString.Lazy as Socket.LBS
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
listen ::
  (Katip.KatipContext m) =>
  -- | An action to run when the daemon is requested to stop
  m () ->
  -- | An action to run when a push request is received
  (Protocol.PushRequest -> Socket.Socket -> m ()) ->
  -- | An active socket to listen on
  Socket.Socket ->
  -- | Returns a socket to the client that requested the daemon to stop
  m Socket.Socket
listen onClientStop onPushRequest sock = loop
  where
    loop = do
      result <- liftIO $ runExceptT $ readPushRequest sock

      case result of
        Right (ClientPing, clientConn) -> do
          Katip.logFM Katip.DebugS "Received ping"
          liftIO $ Socket.LBS.sendAll clientConn (Aeson.encode DaemonPong)
          loop
        Right (ClientStop, clientConn) -> do
          Katip.logFM Katip.DebugS "Received stop request"
          onClientStop
          return clientConn
        Right (ClientPushRequest pushRequest, clientConn) -> do
          onPushRequest pushRequest clientConn
          loop
        Left err@(DecodingError _) -> do
          Katip.logFM Katip.ErrorS $ Katip.ls $ displayException err
          loop
        Left err -> throwIO err

serverBye :: Socket.Socket -> IO ()
serverBye sock =
  Socket.LBS.sendAll sock (Aeson.encode DaemonBye) `catchAny` (\_ -> return ())

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
openSocket :: (MonadIO m) => FilePath -> m Socket.Socket
openSocket socketFilePath = liftIO $ do
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

closeSocket :: (MonadIO m) => Socket.Socket -> m ()
closeSocket = liftIO . Socket.close
