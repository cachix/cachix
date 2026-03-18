module Cachix.DebugInfod.Cache
  ( FetcherCache,
    newFetcherCache,
    fetchOrGet,
    spawnCleanupThread,
  )
where

import Control.Concurrent.STM (TVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Data.Map.Strict qualified as Map
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Protolude hiding (toS)
import Protolude.Conv
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    getModificationTime,
    listDirectory,
    removeDirectoryRecursive,
    renamePath,
  )
import System.FilePath ((</>))
import System.Posix.Files (setFileTimes)
import System.Posix.Types (EpochTime)

-- | A concurrent cache that deduplicates fetches for the same key.
-- Uses per key MVar locking so concurrent requests for the same key
-- wait rather than duplicating work.
data FetcherCache = FetcherCache
  { cacheRoot :: FilePath,
    cacheLocks :: TVar (Map Text (MVar ())),
    cacheExpiration :: Int
  }

-- | Create a new FetcherCache, ensuring the directory structure exists.
newFetcherCache :: FilePath -> Int -> IO FetcherCache
newFetcherCache root expiration = do
  createDirectoryIfMissing True (root </> "cache")
  createDirectoryIfMissing True (root </> "partial")
  locks <- newTVarIO Map.empty
  pure
    FetcherCache
      { cacheRoot = root,
        cacheLocks = locks,
        cacheExpiration = expiration
      }

-- | Get a cached entry or fetch it. Per key locking ensures concurrent
-- requests for the same key wait on the first fetch rather than
-- duplicating work.
fetchOrGet ::
  FetcherCache ->
  Text ->
  (FilePath -> IO Bool) ->
  IO (Maybe FilePath)
fetchOrGet fc key fetch = do
  lock <- getOrCreateLock fc key
  withMVar lock $ \_ -> do
    let cachePath = cacheRoot fc </> "cache" </> toS key
    exists <- doesDirectoryExist cachePath
    if exists
      then do
        touchIfStale fc cachePath
        pure (Just cachePath)
      else do
        let partialPath = cacheRoot fc </> "partial" </> toS key
        createDirectoryIfMissing True partialPath
        result <-
          fetch partialPath `onException` cleanupPartial partialPath
        if result
          then do
            renamePath partialPath cachePath
            pure (Just cachePath)
          else do
            cleanupPartial partialPath
            pure Nothing

-- | Spawn a background thread that periodically removes expired entries.
spawnCleanupThread :: FetcherCache -> IO ()
spawnCleanupThread fc = void $ forkIO $ forever $ do
  threadDelay (cacheExpiration fc * 1000000)
  cleanup fc

-- Internal helpers

getOrCreateLock :: FetcherCache -> Text -> IO (MVar ())
getOrCreateLock fc key = do
  existing <- atomically $ do
    locks <- readTVar (cacheLocks fc)
    pure (Map.lookup key locks)
  case existing of
    Just lock -> pure lock
    Nothing -> do
      lock <- newMVar ()
      atomically $ do
        locks <- readTVar (cacheLocks fc)
        case Map.lookup key locks of
          Just existingLock -> pure existingLock
          Nothing -> do
            writeTVar (cacheLocks fc) (Map.insert key lock locks)
            pure lock

-- | Touch the mtime if the entry is older than half the expiration,
-- so the cleanup thread knows it was recently accessed.
touchIfStale :: FetcherCache -> FilePath -> IO ()
touchIfStale fc path = do
  mtime <- getModificationTime path
  now <- getCurrentTime
  let age = diffUTCTime now mtime
      halfExpiration = fromIntegral (cacheExpiration fc) / 2
  when (age > halfExpiration) $
    touchPath path

touchPath :: FilePath -> IO ()
touchPath path = do
  now <- epochNow
  setFileTimes path now now

epochNow :: IO EpochTime
epochNow = do
  now <- getCurrentTime
  let epoch = utcTimeToPOSIXSeconds now
  pure (fromIntegral (round epoch :: Integer))

cleanup :: FetcherCache -> IO ()
cleanup fc = do
  let cacheDir = cacheRoot fc </> "cache"
  entries <- listDirectory cacheDir `catch` \(_ :: SomeException) -> pure []
  now <- getCurrentTime
  for_ entries $ \entry -> do
    let entryPath = cacheDir </> entry
    void $ try @SomeException $ do
      mtime <- getModificationTime entryPath
      let age = diffUTCTime now mtime
          maxAge = fromIntegral (cacheExpiration fc) * 2
      when (age > maxAge) $ do
        lock <- getOrCreateLock fc (toS entry)
        withMVar lock $ \_ -> do
          -- Re-check mtime under lock in case the entry was touched
          -- between the initial check and acquiring the lock.
          stillExpired <- do
            mtime' <- getModificationTime entryPath
            let age' = diffUTCTime now mtime'
            pure (age' > maxAge)
          when stillExpired $ do
            removeDirectoryRecursive entryPath
            removeLock fc (toS entry)

removeLock :: FetcherCache -> Text -> IO ()
removeLock fc key = atomically $ do
  locks <- readTVar (cacheLocks fc)
  writeTVar (cacheLocks fc) (Map.delete key locks)

cleanupPartial :: FilePath -> IO ()
cleanupPartial path =
  void (try @SomeException $ removeDirectoryRecursive path)
