module Cachix.Client.Push.DebugInfo
  ( scanAndUploadDebugInfo,
  )
where

import Cachix.API qualified as API
import Cachix.Client.Retry (retryHttp)
import Cachix.Client.Servant (cachixClient)
import Cachix.Types.DebugInfo (DebugInfoEntry (..))
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client (Token)
import Servant.Client.Streaming (ClientEnv, runClientM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

-- | Scan a store path for .build-id debug info entries and upload them.
scanAndUploadDebugInfo :: ClientEnv -> Token -> Text -> Text -> FilePath -> IO ()
scanAndUploadDebugInfo clientEnv authToken cacheName narHash storePathDir = do
  entries <- scanDebugInfoEntries storePathDir
  unless (null entries) $ do
    void $
      retryHttp $
        (`runClientM` clientEnv) $
          API.createDebugInfo cachixClient authToken cacheName narHash entries

-- | Scan a store path for lib/debug/.build-id/<XX>/<38hex>.debug entries.
scanDebugInfoEntries :: FilePath -> IO [DebugInfoEntry]
scanDebugInfoEntries storePathDir = do
  let buildIdDir = storePathDir </> "lib" </> "debug" </> ".build-id"
  exists <- doesDirectoryExist buildIdDir
  if not exists
    then pure []
    else do
      prefixDirs <- listDirectory buildIdDir
      fmap concat $
        for prefixDirs $ \prefix -> do
          if isHexPrefix prefix
            then do
              let prefixPath = buildIdDir </> prefix
              isDir <- doesDirectoryExist prefixPath
              if isDir
                then do
                  files <- listDirectory prefixPath
                  pure
                    [ DebugInfoEntry
                        { buildId = toS prefix <> toS (take 38 file),
                          member = "lib/debug/.build-id/" <> toS prefix <> "/" <> toS file
                        }
                    | file <- files,
                      isDebugFile file
                    ]
                else pure []
            else pure []

-- | Check if a directory name is a 2 char hex prefix.
isHexPrefix :: FilePath -> Bool
isHexPrefix [a, b] = isHexDigit a && isHexDigit b
isHexPrefix _ = False

-- | Check if a filename matches <38hex>.debug
isDebugFile :: FilePath -> Bool
isDebugFile name =
  ".debug" `isSuffixOf` name
    && length name == 44 -- 38 hex + ".debug" (6)
    && all isHexDigit (take 38 name)
