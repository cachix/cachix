module Cachix.Client.Daemon.Listen
  ( listen,
    handleClient,
    serverBye,
    getSocketPath,
    openSocket,
    closeSocket,
  )
where

import Cachix.Client.Config.Orphans ()
import qualified Cachix.Client.Daemon.EventLoop as EventLoop
import Cachix.Client.Daemon.Protocol as Protocol
import Cachix.Client.Daemon.Types (DaemonError, toExitCodeInt)
import Cachix.Client.Daemon.Types.EventLoop (EventLoop)
import Cachix.Client.Daemon.Types.SocketStore (SocketId)
import Control.Exception.Safe (catchAny)
import qualified Control.Monad.Catch as E
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Katip
import Network.Socket (Socket)
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
import System.IO.Error (isDoesNotExistError, isResourceVanishedError)

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
  EventLoop a ->
  FilePath ->
  m ()
listen eventloop daemonSocketPath = forever $ do
  E.bracketOnError (openSocket daemonSocketPath) closeSocket $ \sock -> do
    liftIO $ Socket.listen sock Socket.maxListenQueue
    (conn, _peerAddr) <- liftIO $ Socket.accept sock
    EventLoop.send eventloop (EventLoop.AddSocketClient conn)

-- | Handle incoming messages from a client.
--
-- Automatically responds to pings.
-- Requests the daemon to remove the client socket once the loop exits.
handleClient ::
  forall m a.
  (E.MonadMask m, Katip.KatipContext m) =>
  EventLoop a ->
  SocketId ->
  Socket ->
  m ()
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
            EventLoop.send eventloop (EventLoop.ReceivedMessage msg)
            case msg of
              Protocol.ClientPing ->
                liftIO $ Socket.LBS.sendAll conn $ Protocol.newMessage DaemonPong
              _ -> return ()

          go newLeftovers

    removeClient = EventLoop.send eventloop (EventLoop.RemoveSocketClient socketId)

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
  Socket.LBS.sendAll sock (Protocol.newMessage (DaemonBye exitCode)) `catchAny` (\_ -> return ())
  where
    exitCode = toExitCodeInt exitResult

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
