module Cachix.Client.Command.Doctor (doctor) where

import Cachix.API qualified as API
import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.OptionsParser (DoctorOptions (..))
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant (cachixClient, isErr)
import Cachix.Daemon.Listen (getSocketPath)
import Cachix.Daemon.Protocol qualified as Protocol
import Cachix.Types.BinaryCache (BinaryCache (..))
import Cachix.Types.Permission (Permission (..))
import Control.Exception.Safe qualified as Exception
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Network.HTTP.Types (status401, status404)
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as Socket.BS
import Network.Socket.ByteString.Lazy qualified as Socket.LBS
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client
import Servant.Client.Streaming (ClientError, runClientM)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Timeout (timeout)

-- | Result of a single check
data CheckResult
  = CheckOK
  | CheckFailed Text
  | CheckWarning Text
  deriving (Show)

-- | Result of checking a cache
data CacheCheckResult = CacheCheckResult
  { cacheResultName :: Text,
    cacheResultUri :: Maybe Text,
    cacheResultPublic :: Maybe Bool,
    cacheResultPermission :: Maybe Permission,
    cacheResultHasSigningKey :: Bool,
    cacheResultConnectivity :: CheckResult,
    cacheResultAuth :: CheckResult
  }

-- | Result of checking daemon
data DaemonCheckResult
  = DaemonNotRunning
  | DaemonRunning FilePath (Maybe Protocol.DaemonDiagnostics)
  | DaemonConnectionFailed Text
  deriving (Show)

doctor :: Env -> DoctorOptions -> IO ()
doctor env opts = do
  -- Resolve socket path: CLI option takes precedence over environment variable
  envSocketPath <- fmap toS <$> lookupEnv "CACHIX_DAEMON_SOCKET"
  let socketPath = doctorSocketPath opts <|> envSocketPath
      socketExplicit = isJust socketPath

  -- Validate options: socket and CACHE-NAME are mutually exclusive
  case (socketPath, doctorCacheName opts) of
    (Just _, Just _) -> do
      putErrText "Error: --socket/CACHIX_DAEMON_SOCKET and CACHE-NAME cannot be used together."
      putErrText "When using a socket, the daemon determines which cache to check."
      exitFailure
    _ -> return ()

  putStrLn ("Cachix Doctor" :: Text)
  putStrLn ("=============" :: Text)
  putStrLn ("" :: Text)

  -- Daemon check first (needed to decide config check behavior)
  daemonResult <- checkDaemon socketPath

  -- Configuration checks (skip if using daemon successfully)
  configResult <- case (socketPath, daemonResult) of
    (Just _, DaemonRunning _ (Just _)) ->
      return
        ConfigCheckResult
          { configFileResult = CheckOK,
            authTokenResult = CheckOK,
            authTokenSource = Just "via daemon"
          }
    _ -> checkConfiguration env
  printConfigResult configResult

  -- Auth check
  authResult <- checkAuth env
  printAuthResult authResult

  -- Daemon status
  printDaemonResult daemonResult

  -- Cache checks - use daemon diagnostics only if socket explicitly provided and daemon responded
  cacheResults <- case (socketPath, daemonResult) of
    (Just _, DaemonRunning _ (Just diag)) -> return [diagnosticsToCacheResult diag]
    _ -> checkCaches env opts
  printCacheResults cacheResults

  -- Store path check (if provided)
  storePathResult <- case doctorStorePath opts of
    Just storePath -> do
      result <- checkStorePath env opts storePath
      printStorePathResult result
      return (Just result)
    Nothing -> return Nothing

  -- Summary
  let allPassed = configPassed configResult && authPassed authResult && cachesPassed cacheResults && storePathOk storePathResult && daemonOk socketExplicit daemonResult
  putStrLn ("" :: Text)
  if allPassed
    then putStrLn ("All checks passed." :: Text)
    else do
      putStrLn ("Some checks failed." :: Text)
      exitFailure

-- | Convert daemon diagnostics to cache check result for unified rendering
diagnosticsToCacheResult :: Protocol.DaemonDiagnostics -> CacheCheckResult
diagnosticsToCacheResult diag =
  CacheCheckResult
    { cacheResultName = Protocol.diagCacheName diag,
      cacheResultUri = Just (Protocol.diagCacheUri diag),
      cacheResultPublic = Just (Protocol.diagCachePublic diag),
      cacheResultPermission = Nothing, -- Daemon doesn't report permission level
      cacheResultHasSigningKey = Protocol.diagHasSigningKey diag,
      cacheResultConnectivity = CheckOK, -- Daemon is connected
      cacheResultAuth =
        if Protocol.diagAuthOk diag
          then CheckOK
          else CheckFailed (fromMaybe "authentication failed" (Protocol.diagError diag))
    }

-- Configuration checks
data ConfigCheckResult = ConfigCheckResult
  { configFileResult :: CheckResult,
    authTokenResult :: CheckResult,
    authTokenSource :: Maybe Text
  }

configPassed :: ConfigCheckResult -> Bool
configPassed ConfigCheckResult {..} =
  case (configFileResult, authTokenResult) of
    (CheckFailed _, _) -> False
    (_, CheckFailed _) -> False
    _ -> True -- Warnings are OK for both config file and auth token

checkConfiguration :: Env -> IO ConfigCheckResult
checkConfiguration env = do
  let configPath = Config.configPath (cachixoptions env)

  -- Check config file
  configExists <- doesFileExist configPath
  let configFileRes =
        if configExists
          then CheckOK
          else CheckWarning "Config file not found (using defaults)"

  -- Check auth token
  maybeToken <- Config.getAuthTokenMaybe (config env)
  let (authRes, authSource) = case maybeToken of
        Just (Token t) | not (T.null (toS t)) -> (CheckOK, Just "from config")
        _ -> (CheckWarning "No auth token configured", Nothing)

  return
    ConfigCheckResult
      { configFileResult = configFileRes,
        authTokenResult = authRes,
        authTokenSource = authSource
      }

printConfigResult :: ConfigCheckResult -> IO ()
printConfigResult ConfigCheckResult {..} = do
  putStrLn ("Configuration" :: Text)
  printCheck "Config file" configFileResult Nothing
  printCheck "Auth token" authTokenResult authTokenSource

-- Auth validation
authPassed :: CheckResult -> Bool
authPassed CheckOK = True
authPassed (CheckWarning _) = True
authPassed (CheckFailed _) = False

checkAuth :: Env -> IO CheckResult
checkAuth _env = do
  -- Auth is validated per-cache, so just return OK here
  -- The actual auth validation happens in checkCaches
  return CheckOK

printAuthResult :: CheckResult -> IO ()
printAuthResult _ = return () -- Auth is shown per-cache

-- Cache checks
cachesPassed :: [CacheCheckResult] -> Bool
cachesPassed = all cacheCheckPassed
  where
    cacheCheckPassed CacheCheckResult {..} =
      case (cacheResultConnectivity, cacheResultAuth) of
        (CheckOK, CheckOK) -> True
        (CheckOK, CheckWarning _) -> True
        _ -> False

checkCaches :: Env -> DoctorOptions -> IO [CacheCheckResult]
checkCaches env opts = do
  let configuredCaches = Config.binaryCaches (config env)

  case doctorCacheName opts of
    Just specificCache -> do
      -- Check specific cache
      result <- checkCache env specificCache configuredCaches
      return [result]
    Nothing ->
      if null configuredCaches
        then do
          putStrLn ("" :: Text)
          putStrLn ("No caches configured." :: Text)
          return []
        else do
          -- Check all configured caches
          forM configuredCaches $ \cacheConfig ->
            checkCache env (Config.name cacheConfig) configuredCaches

checkCache :: Env -> Text -> [Config.BinaryCacheConfig] -> IO CacheCheckResult
checkCache env cacheName configuredCaches = do
  maybeToken <- Config.getAuthTokenMaybe (config env)
  let token = fromMaybe (Token "") maybeToken

  -- Check if we have a signing key for this cache
  let hasSigningKey = any (\c -> Config.name c == cacheName && not (T.null (Config.secretKey c))) configuredCaches

  -- Try to get cache info (validates auth and connectivity)
  cacheRes <- retryHttp $ (`runClientM` clientenv env) $ API.getCache cachixClient token cacheName

  case cacheRes of
    Right bc ->
      return
        CacheCheckResult
          { cacheResultName = cacheName,
            cacheResultUri = Just (uri bc),
            cacheResultPublic = Just (isPublic bc),
            cacheResultPermission = Just (permission bc),
            cacheResultHasSigningKey = hasSigningKey,
            cacheResultConnectivity = CheckOK,
            cacheResultAuth = CheckOK
          }
    Left err ->
      return
        CacheCheckResult
          { cacheResultName = cacheName,
            cacheResultUri = Nothing,
            cacheResultPublic = Nothing,
            cacheResultPermission = Nothing,
            cacheResultHasSigningKey = hasSigningKey,
            cacheResultConnectivity = checkConnectivityError err maybeToken,
            cacheResultAuth = checkAuthError err maybeToken
          }

checkConnectivityError :: ClientError -> Maybe Token -> CheckResult
checkConnectivityError err maybeToken
  | isErr err status404 = CheckFailed "Cache not found"
  | isErr err status401 = case maybeToken of
      Just _ -> CheckOK -- Auth error, not connectivity
      Nothing -> CheckFailed "Authentication required"
  | otherwise = CheckFailed (toS (show err :: [Char]))

checkAuthError :: ClientError -> Maybe Token -> CheckResult
checkAuthError err maybeToken
  | isErr err status401 = case maybeToken of
      Just _ -> CheckFailed "Invalid or expired auth token"
      Nothing -> CheckWarning "Not authenticated (cache may be private)"
  | isErr err status404 = CheckOK -- Not an auth error
  | otherwise = CheckOK

printCacheResults :: [CacheCheckResult] -> IO ()
printCacheResults results = do
  forM_ results $ \result -> do
    putStrLn ("" :: Text)
    putStrLn ("Cache: " <> cacheResultName result)
    case cacheResultUri result of
      Just u -> putStrLn ("  URI: " <> u)
      Nothing -> return ()
    case cacheResultPublic result of
      Just True -> putStrLn ("  Public: yes" :: Text)
      Just False -> putStrLn ("  Public: no" :: Text)
      Nothing -> return ()
    case cacheResultPermission result of
      Just p -> putStrLn ("  Permission: " <> T.toLower (show p))
      Nothing -> return ()
    let signingKeyResult = if cacheResultHasSigningKey result then CheckOK else CheckWarning "not configured"
    printCheck "Signing key" signingKeyResult Nothing
    printCheck "Connectivity" (cacheResultConnectivity result) Nothing
    printCheck "Authentication" (cacheResultAuth result) Nothing

-- Store path checks
data StorePathCheckResult = StorePathCheckResult
  { storePathQuery :: Text,
    storePathHash :: Text,
    storePathCacheName :: Text,
    storePathStatus :: StorePathStatus
  }
  deriving (Show)

data StorePathStatus
  = InCache
  | NotInCache
  | StorePathCheckError Text
  deriving (Show)

storePathOk :: Maybe StorePathCheckResult -> Bool
storePathOk Nothing = True
storePathOk (Just StorePathCheckResult {..}) =
  case storePathStatus of
    InCache -> True
    NotInCache -> True -- Not an error, just informational
    StorePathCheckError _ -> False

-- | Extract the hash from a store path
-- Store paths look like: /nix/store/abc123...-name
-- The hash is the 32-character string after /nix/store/
extractStoreHash :: Text -> Maybe Text
extractStoreHash storePath =
  let path = T.strip storePath
      -- Handle both full paths and just hashes
      normalized =
        if "/nix/store/" `T.isPrefixOf` path
          then T.drop 11 path -- Drop "/nix/store/"
          else path
   in if T.length normalized >= 32
        then Just $ T.take 32 normalized
        else Nothing

checkStorePath :: Env -> DoctorOptions -> Text -> IO StorePathCheckResult
checkStorePath env opts storePath = do
  maybeToken <- Config.getAuthTokenMaybe (config env)
  let token = fromMaybe (Token "") maybeToken
  let configuredCaches = Config.binaryCaches (config env)

  -- Determine which cache to check
  let cacheName = case doctorCacheName opts of
        Just name -> name
        Nothing -> case configuredCaches of
          (first' : _) -> Config.name first'
          [] -> "cachix" -- Default fallback
  case extractStoreHash storePath of
    Nothing ->
      return
        StorePathCheckResult
          { storePathQuery = storePath,
            storePathHash = "",
            storePathCacheName = cacheName,
            storePathStatus = StorePathCheckError "Invalid store path format"
          }
    Just storeHash -> do
      -- Use narinfoBulk to check if the path exists
      result <- retryHttp $ (`runClientM` clientenv env) $ API.narinfoBulk cachixClient token cacheName [storeHash]
      case result of
        Right missingHashes ->
          let status = if storeHash `elem` missingHashes then NotInCache else InCache
           in return
                StorePathCheckResult
                  { storePathQuery = storePath,
                    storePathHash = storeHash,
                    storePathCacheName = cacheName,
                    storePathStatus = status
                  }
        Left err ->
          return
            StorePathCheckResult
              { storePathQuery = storePath,
                storePathHash = storeHash,
                storePathCacheName = cacheName,
                storePathStatus = StorePathCheckError (toS (show err :: [Char]))
              }

printStorePathResult :: StorePathCheckResult -> IO ()
printStorePathResult StorePathCheckResult {..} = do
  putStrLn ("" :: Text)
  putStrLn ("Store Path" :: Text)
  putStrLn ("  Query: " <> storePathQuery)
  putStrLn ("  Hash: " <> storePathHash)
  putStrLn ("  Cache: " <> storePathCacheName)
  case storePathStatus of
    InCache ->
      printCheck "Status" CheckOK (Just "found in cache")
    NotInCache ->
      printCheck "Status" (CheckWarning "not in cache") Nothing
    StorePathCheckError err ->
      printCheck "Status" (CheckFailed err) Nothing

-- Daemon checks

-- | Check if daemon status is OK
-- If socket was explicitly requested (--socket), failures are errors
-- Otherwise, daemon issues are just informational
daemonOk :: Bool -> DaemonCheckResult -> Bool
daemonOk socketExplicit result = case result of
  DaemonNotRunning -> not socketExplicit -- Fail if socket explicitly requested but doesn't exist
  DaemonRunning _ maybeDiag -> maybe True Protocol.diagAuthOk maybeDiag
  DaemonConnectionFailed _ -> not socketExplicit -- Fail if socket explicitly requested but can't connect

checkDaemon :: Maybe FilePath -> IO DaemonCheckResult
checkDaemon optionalSocketPath = do
  socketPath <- maybe getSocketPath pure optionalSocketPath
  exists <- doesFileExist socketPath

  if not exists
    then return DaemonNotRunning
    else do
      -- Try to connect and get diagnostics
      result <- Exception.try $ queryDaemonDiagnostics socketPath
      case result of
        Right (Just diag) -> return $ DaemonRunning socketPath (Just diag)
        Right Nothing -> return $ DaemonConnectionFailed "No response from daemon"
        Left (e :: SomeException) -> return $ DaemonConnectionFailed (toS $ displayException e)

queryDaemonDiagnostics :: FilePath -> IO (Maybe Protocol.DaemonDiagnostics)
queryDaemonDiagnostics socketPath =
  Exception.bracket
    (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
    Socket.close
    ( \sock -> do
        Socket.connect sock (Socket.SockAddrUnix socketPath)
        -- Send diagnostics request
        Socket.LBS.sendAll sock $ Protocol.newMessage Protocol.ClientDiagnosticsRequest
        -- Wait for response with timeout (10 seconds - auth check may take time)
        result <- timeout 10000000 $ receiveResponse sock
        case result of
          Just (Just (Protocol.DaemonDiagnosticsResult diag)) -> return (Just diag)
          _ -> return Nothing
    )

receiveResponse :: Socket.Socket -> IO (Maybe Protocol.DaemonMessage)
receiveResponse sock = do
  bs <- Socket.BS.recv sock 4096
  if BS.null bs
    then return Nothing
    else do
      let (msgs, _) = Protocol.splitMessages bs
      case msgs of
        (msg : _) -> return $ decodeMsg msg
        [] -> return Nothing
  where
    decodeMsg msg = case Aeson.eitherDecodeStrict msg of
      Right m -> Just m
      Left _ -> Nothing

printDaemonResult :: DaemonCheckResult -> IO ()
printDaemonResult result = do
  putStrLn ("" :: Text)
  putStrLn ("Daemon" :: Text)
  case result of
    DaemonNotRunning ->
      printCheck "Status" (CheckWarning "not running") Nothing
    DaemonRunning socketPath _ -> do
      printCheck "Status" CheckOK (Just "running")
      putStrLn ("  Socket: " <> (toS socketPath :: Text))
    DaemonConnectionFailed reason ->
      printCheck "Status" (CheckFailed reason) Nothing

-- Helper to print a check result
printCheck :: Text -> CheckResult -> Maybe Text -> IO ()
printCheck name result extra = do
  let (symbol, status) = case result of
        CheckOK -> ("✓", "")
        CheckWarning msg -> ("!", msg)
        CheckFailed msg -> ("✗", msg)
      extraText = case (extra, status) of
        (Just e, "") -> e
        (Just e, s) -> s <> " (" <> e <> ")"
        (Nothing, s) -> s
      statusText = if T.null extraText then "" else " " <> extraText
  putStrLn $ "  " <> symbol <> " " <> name <> statusText
