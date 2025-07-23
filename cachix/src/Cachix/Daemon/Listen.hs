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
import Cachix.Daemon.Types.SocketStore (SocketId)
import Control.Exception.Safe (catchAny)
import Control.Monad.Catch qualified as E
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Katip qualified
import Network.Socket (Socket)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.BS
import Network.Socket.ByteString.Lazy qualified as Socket.LBS
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
import Cachix.Daemon.Types.SocketStore (SocketId, SocketStore(..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Cachix.Daemon.SocketStore (addSocket, removeSocket)

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
listen :: (E.MonadMask m, Katip.KatipContext m, MonadUnliftIO m) => EventLoop DaemonEvent a -> FilePath -> SocketStore -> m ()
listen eventloop daemonSocketPath socketStore = do
  sock <- openSocket daemonSocketPath
  E.bracket (pure sock) closeSocket $ \sock' -> do
    liftIO $ Socket.listen sock' Socket.maxListenQueue
    forever $ do
      (conn, _peerAddr) <- liftIO $ Socket.accept sock'
      socketId <- addSocket conn (handleClient eventloop) socketStore
      EventLoop.send eventloop (AddSocketClient conn socketId)

-- | Handle incoming messages from a client.
--
-- Automatically responds to pings.
-- Requests the daemon to remove the client socket once the loop exits.
handleClient :: (E.MonadMask m, Katip.KatipContext m) => EventLoop DaemonEvent a -> SocketId -> Socket -> m ()
handleClient eventloop socketId conn = do
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
            EventLoop.send eventloop (ReceivedMessage msg socketId)
            case msg of
              Protocol.ClientPing -> liftIO $ Socket.LBS.sendAll conn $ Protocol.newMessage DaemonPong
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

serverBye :: Socket.Socket -> Either DaemonError () -> IO ()
serverBye sock exitResult =
  Socket.LBS.sendAll sock (Protocol.newMessage (DaemonExit exitStatus)) `catchAny` (\_ -> return ())
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
