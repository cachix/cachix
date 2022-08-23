module Cachix.Deploy.Lock (withTryLock) where

import qualified Lukko as Lock
import Protolude hiding ((<.>))
import qualified System.Directory as Directory
import System.FilePath ((<.>), (</>))
import qualified System.FilePath as FilePath
import qualified System.Posix.Files as Files
import qualified System.Posix.Types as Posix

defaultLockDirectory :: FilePath
defaultLockDirectory = "cachix" </> "deploy" </> "locks"

-- | Get the path to the lock directory based on the ownership of the profile.
--
-- For user-owned profiles: $XDG_CACHE_DIR/cachix/deploy/locks
-- For root-owned profiles: /var/run/cachix/deploy/locks
getLockDirectoryFromProfile :: FilePath -> IO FilePath
getLockDirectoryFromProfile profilePath = do
  userId <- Files.fileOwner <$> Files.getFileStatus (FilePath.takeDirectory profilePath)
  if isRoot userId
    then pure $ "/var/run" </> defaultLockDirectory
    else Directory.getXdgDirectory Directory.XdgCache defaultLockDirectory
  where
    isRoot :: Posix.UserID -> Bool
    isRoot = (==) 0

-- | Run an IO action with an acquired profile lock. Returns immediately if the profile is already locked.
--
-- Lock files are stored in either the user’s or system’s cache directory,
-- depending on the ownership of the profile.
--
-- Lock files are not deleted after use.
withTryLock :: FilePath -> IO a -> IO (Maybe a)
withTryLock profilePath action = do
  lockDirectory <- getLockDirectoryFromProfile profilePath

  Directory.createDirectoryIfMissing True lockDirectory
  Directory.setPermissions lockDirectory $
    Directory.emptyPermissions
      & Directory.setOwnerReadable True
      & Directory.setOwnerWritable True
      & Directory.setOwnerExecutable True
      & Directory.setOwnerSearchable True

  let profile = FilePath.takeFileName profilePath
  let lockFile = lockDirectory </> profile <.> "lock"

  bracket
    (Lock.fdOpen lockFile)
    (Lock.fdUnlock *> Lock.fdClose)
    $ \fd -> do
      isLocked <- Lock.fdTryLock fd Lock.ExclusiveLock
      if isLocked
        then Just <$> action
        else pure Nothing
