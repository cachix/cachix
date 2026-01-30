module Cachix.Daemon.Listen
  ( listen,
    handleClient,
    serverBye,
    getSocketPath,
    openSocket,
    closeSocket,
  )
where

import Cachix.Client.Config.Orphans ()
import Cachix.Daemon.EventLoop qualified as EventLoop
import Cachix.Daemon.Protocol as Protocol
import Cachix.Daemon.SocketStore qualified as SocketStore
import Cachix.Daemon.Types
  ( DaemonError,
    DaemonEvent
      ( AddSocketClient,
        ReceivedMessage,
        RemoveSocketClient
      ),
    toExitCodeInt,
  )
import Cachix.Daemon.Types.EventLoop (EventLoop)
import Cachix.Daemon.Types.SocketStore (SocketId, SocketStore)
import Control.Exception.Safe (catchAny)
import Control.Monad.Catch qualified as E
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Katip qualified
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.BS
import Protolude
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    getXdgDirectory,
    removeFile,
  )
import System.Environment qualified as System
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError, isResourceVanishedError)
import System.Posix.Files (setFileMode)

-- TODO: reconcile with Client
data ListenError
  = SocketError SomeException
  | DecodingError Text
  deriving stock (Show)

instance Exception ListenError where
  displayException = \case
    SocketError err -> "Failed to read from the daemon socket: " <> show err
    DecodingError err -> "Failed to decode request:\n" <> toS err

-- | Listen for incoming connections on the given socket path.
listen ::
  (E.MonadMask m, Katip.KatipContext m) =>
  EventLoop DaemonEvent a ->
  FilePath ->
  m ()
listen eventloop daemonSocketPath = do
  sock <- openSocket daemonSocketPath
  E.bracket (pure sock) closeSocket $ \sock' -> do
    liftIO $ Socket.listen sock' Socket.maxListenQueue
    forever $ do
      (conn, _peerAddr) <- liftIO $ Socket.accept sock'
      EventLoop.send eventloop (AddSocketClient conn)

-- | Handle incoming messages from a client.
--
-- Automatically responds to pings.
-- Requests the daemon to remove the client socket once the loop exits.
handleClient ::
  forall m a.
  (E.MonadMask m, Katip.KatipContext m) =>
  EventLoop DaemonEvent a ->
  SocketStore ->
  SocketId ->
  Socket ->
  m ()
handleClient eventloop socketStore socketId conn = do
  go BS.empty `E.finally` removeClient
  where
    go leftovers = do
      ebs <- liftIO $ try $ Socket.BS.recv conn 8192

      case ebs of
        Left err | isResourceVanishedError err -> return ()
        Left _ -> return ()
        -- If the socket returns 0 bytes, then it is closed
        Right bs | BS.null bs -> return ()
        Right bs -> do
          let (rawMsgs, newLeftovers) = Protocol.splitMessages (BS.append leftovers bs)
          msgs <- catMaybes <$> mapM decodeMessage rawMsgs

          forM_ msgs $ \msg -> do
            EventLoop.send eventloop (ReceivedMessage socketId msg)
            case msg of
              Protocol.ClientPing ->
                liftIO $ SocketStore.sendAll socketStore socketId (Protocol.newMessage DaemonPong)
              _ -> return ()

          go newLeftovers

    removeClient = EventLoop.send eventloop (RemoveSocketClient socketId)

decodeMessage :: (Katip.KatipContext m) => ByteString -> m (Maybe Protocol.ClientMessage)
decodeMessage "" = return Nothing
decodeMessage bs =
  case Aeson.eitherDecodeStrict bs of
    Left err -> do
      Katip.logFM Katip.ErrorS $ Katip.ls $ displayException (DecodingError (toS err))
      return Nothing
    Right msg -> return (Just msg)

serverBye :: SocketId -> SocketStore -> Either DaemonError () -> IO ()
serverBye socketId socketStore exitResult =
  SocketStore.sendAll socketStore socketId (Protocol.newMessage (DaemonExit exitStatus)) `catchAny` (\_ -> return ())
  where
    exitStatus = DaemonExitStatus {exitCode, exitMessage}
    exitCode = toExitCodeInt exitResult
    exitMessage =
      case exitResult of
        Left err -> Just (toS $ displayException err)
        Right _ -> Nothing

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
  setFileMode socketFilePath 0o666
  return sock
  where
    deleteSocketFileIfExists path =
      removeFile path `catch` handleDoesNotExist

    handleDoesNotExist e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

closeSocket :: (MonadIO m) => Socket.Socket -> m ()
closeSocket = liftIO . Socket.close
