module Cachix.Daemon.DryMode
  ( mockBinaryCache,
    mockPushSecret,
    dryRunProcessBatch,
  )
where

import Cachix.Client.Push (PushSecret (..))
import Cachix.Types.BinaryCache (BinaryCache (..), BinaryCacheName)
import Cachix.Types.BinaryCache qualified as BinaryCache
import Cachix.Types.Permission qualified as Permission
import Hercules.CNix.Store (StorePath)
import Protolude
import Servant.Auth.Client (Token (..))

-- | A mock BinaryCache for dry run.
-- Uses sensible defaults without making network requests.
mockBinaryCache :: BinaryCacheName -> BinaryCache
mockBinaryCache cacheName =
  BinaryCache
    { name = cacheName,
      uri = "https://" <> cacheName <> ".cachix.org",
      isPublic = True,
      publicSigningKeys = [],
      githubUsername = "",
      permission = Permission.Write,
      preferredCompressionMethod = BinaryCache.ZSTD
    }

-- | A mock PushSecret for dry mode.
-- Uses an empty token since no auth is needed.
mockPushSecret :: PushSecret
mockPushSecret = PushToken (Token "")

-- | Mock batch processor that assumes all paths are already present.
-- Returns empty lists for both closure and missing paths, which means
-- all paths will be treated as already present in the cache.
dryRunProcessBatch :: [StorePath] -> IO ([StorePath], [StorePath])
dryRunProcessBatch _storePaths = pure ([], [])
