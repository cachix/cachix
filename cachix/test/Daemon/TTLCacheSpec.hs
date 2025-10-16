module Daemon.TTLCacheSpec where

import Cachix.Daemon.TTLCache (TTLCache)
import Cachix.Daemon.TTLCache qualified as TTLCache
import Data.Time (addUTCTime, getCurrentTime)
import Protolude
import Test.Hspec

spec :: Spec
spec = do
  describe "TTL cache" $ do
    it "correctly handles expiration" $ do
      now <- getCurrentTime
      let past = addUTCTime (-10) now -- 10 seconds ago
          future = addUTCTime 10 now -- 10 seconds from now
          cache :: TTLCache Text
          cache =
            TTLCache.insert "expired" past $
              TTLCache.insert "valid" future TTLCache.empty

      -- Expired entry should not be found
      TTLCache.lookup now "expired" cache `shouldBe` False
      -- Valid entry should be found
      TTLCache.lookup now "valid" cache `shouldBe` True

    it "respects size limits when pruning" $ do
      now <- getCurrentTime
      let future = addUTCTime 60 now
          -- Create cache with 5 entries
          cache :: TTLCache Text
          cache = foldl' (\c i -> TTLCache.insert (show i) future c) TTLCache.empty [1 .. 5 :: Integer]

      TTLCache.size cache `shouldBe` 5

      -- Prune to 3 entries
      let pruned = TTLCache.pruneToSize 3 cache
      TTLCache.size pruned `shouldBe` 3
