module Cachix.Deploy.Lock (withTryLock) where

import qualified Lukko as Lock
import Protolude hiding ((<.>))
import qualified System.Directory as Directory
import System.FilePath ((<.>), (</>))

defaultLockDirectory :: FilePath
defaultLockDirectory = "cachix" </> "deploy" </> "locks"

-- | Run an IO action with an acquired profile lock. Returns immediately if the profile is already locked.
--
-- Lock files are not deleted after use.
--
-- macOS: if using sudo, make sure to use `-H` to reset the home directory.
withTryLock :: Text -> IO a -> IO (Maybe a)
withTryLock profileName action = do
  lockDirectory <- Directory.getXdgDirectory Directory.XdgCache defaultLockDirectory

  Directory.createDirectoryIfMissing True lockDirectory
  Directory.setPermissions lockDirectory $
    Directory.emptyPermissions
      & Directory.setOwnerReadable True
      & Directory.setOwnerWritable True
      & Directory.setOwnerExecutable True
      & Directory.setOwnerSearchable True

  let lockFile = lockDirectory </> toS profileName <.> "lock"

  bracket
    (Lock.fdOpen lockFile)
    (Lock.fdUnlock *> Lock.fdClose)
    $ \fd -> do
      isLocked <- Lock.fdTryLock fd Lock.ExclusiveLock
      if isLocked
        then Just <$> action
        else pure Nothing
