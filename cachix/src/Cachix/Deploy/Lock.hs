module Cachix.Deploy.Lock
  ( defaultLockDirectory,
    getLockDirectory,
    readPidFile,
    withTryLock,
    withTryLockAndPid,
    lockExtension,
    pidExtension,
  )
where

import qualified Lukko as Lock
import Protolude hiding ((<.>))
import qualified System.Directory as Directory
import System.FilePath ((<.>), (</>))
import System.Posix (getProcessID)
import System.Posix.Types (CPid (..))

lockExtension :: FilePath
lockExtension = "lock"

pidExtension :: FilePath
pidExtension = "pid"

defaultLockDirectory :: FilePath
defaultLockDirectory = "cachix" </> "deploy" </> "locks"

getLockDirectory :: IO FilePath
getLockDirectory = do
  lockDirectory <- Directory.getXdgDirectory Directory.XdgCache defaultLockDirectory

  Directory.createDirectoryIfMissing True lockDirectory
  Directory.setPermissions lockDirectory $
    Directory.emptyPermissions
      & Directory.setOwnerReadable True
      & Directory.setOwnerWritable True
      & Directory.setOwnerExecutable True
      & Directory.setOwnerSearchable True

  pure lockDirectory

readPidFile :: FilePath -> IO (Maybe CPid)
readPidFile pidFile = readMaybe <$> readFile pidFile

-- | Run an IO action with an acquired profile lock.
-- Returns immediately if the profile is already locked.
-- Lock files are not deleted after use.
--
-- macOS: if using sudo, make sure to use `-H` to reset the home directory.
withTryLock :: FilePath -> IO a -> IO (Maybe a)
withTryLock lockFile action = do
  bracket
    (Lock.fdOpen lockFile)
    (Lock.fdUnlock *> Lock.fdClose)
    $ \fd -> do
      isLocked <- Lock.fdTryLock fd Lock.ExclusiveLock
      if isLocked
        then fmap Just action
        else pure Nothing

withTryLockAndPid :: FilePath -> FilePath -> IO a -> IO (Maybe a)
withTryLockAndPid lockFile pidFile action = do
  withTryLock lockFile $ do
    CPid pid <- getProcessID
    writeFile pidFile (show pid)
    action
