{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cachix.Daemon.TTLCache
  ( -- * Types
    TTLCache (..),

    -- * Operations
    empty,
    insert,
    lookup,
    cleanupExpired,
    pruneToSize,
    size,
  )
where

import Data.HashMap.Strict qualified as HashMap
import Data.PQueue.Min qualified as PQ
import Data.Time (UTCTime)
import Protolude hiding (empty)

-- | TTL cache with efficient expiration using priority queue
data TTLCache k = TTLCache
  { -- | Fast lookups by key
    tcLookupMap :: !(HashMap.HashMap k UTCTime),
    -- | Min-heap ordered by expiration time for efficient cleanup
    tcExpirationQueue :: !(PQ.MinQueue (UTCTime, k))
  }
  deriving stock (Eq, Show)

-- | Create an empty TTL cache O(1)
empty :: TTLCache k
empty = TTLCache HashMap.empty PQ.empty

-- | Lookup a value in the TTL cache, checking expiration O(log n)
lookup :: (Ord k, Hashable k) => UTCTime -> k -> TTLCache k -> Bool
lookup now key TTLCache {tcLookupMap} =
  case HashMap.lookup key tcLookupMap of
    Nothing -> False
    Just expiresAt -> now < expiresAt

-- | Get the current size of the cache O(1)
size :: TTLCache k -> Int
size = PQ.size . tcExpirationQueue

-- | Insert a value into the TTL cache with expiration time O(log n)
insert :: (Ord k, Hashable k) => k -> UTCTime -> TTLCache k -> TTLCache k
insert key expiresAt TTLCache {tcLookupMap, tcExpirationQueue} =
  TTLCache
    { tcLookupMap = HashMap.insert key expiresAt tcLookupMap,
      tcExpirationQueue = PQ.insert (expiresAt, key) tcExpirationQueue
    }

-- | Remove expired entries from the cache
cleanupExpired :: (Ord k, Hashable k) => UTCTime -> TTLCache k -> TTLCache k
cleanupExpired now cache@TTLCache {tcLookupMap, tcExpirationQueue} =
  let (expired, remaining) = PQ.span ((<= now) . fst) tcExpirationQueue
      expiredKeys = map snd expired
      newLookupMap = foldr HashMap.delete tcLookupMap expiredKeys
   in cache {tcLookupMap = newLookupMap, tcExpirationQueue = remaining}

-- | Prune cache to target size by removing oldest entries
pruneToSize :: (Ord k, Hashable k) => Int -> TTLCache k -> TTLCache k
pruneToSize targetSize cache@TTLCache {tcLookupMap, tcExpirationQueue}
  | HashMap.size tcLookupMap <= targetSize = cache
  | otherwise =
      let excessCount = HashMap.size tcLookupMap - targetSize
          (toRemove, remaining) = PQ.splitAt excessCount tcExpirationQueue
          keysToRemove = map snd toRemove
          newLookupMap = foldr HashMap.delete tcLookupMap keysToRemove
       in cache {tcLookupMap = newLookupMap, tcExpirationQueue = remaining}
