module Cachix.DebugInfod.StorePath
  ( StorePath (..),
    parseStorePath,
    storePathRelative,
    demangle,
  )
where

import Data.Char (isAsciiLower)
import Data.Text qualified as T
import Protolude hiding (toS)
import Protolude.Conv

nixStore :: Text
nixStore = "/nix/store"

hashLen :: Int
hashLen = 32

-- | A parsed Nix store path.
data StorePath = StorePath
  { storePathFull :: Text,
    storePathName :: Text,
    storePathHash :: Text
  }
  deriving (Show, Eq)

-- | Parse and validate a Nix store path.
-- Must start with /nix/store/ and have a valid hash-name component.
parseStorePath :: Text -> Either Text StorePath
parseStorePath path
  | not (nixStore `T.isPrefixOf` path) =
      Left $ "does not start with " <> nixStore
  | otherwise =
      let stripped = T.drop (T.length nixStore + 1) path
          name = T.takeWhile (/= '/') stripped
          hashPart = T.take hashLen name
       in if T.length name < hashLen + 2
            then Left "store path does not have a hash"
            else
              if not (T.all isNixBase32Char hashPart)
                then Left "invalid hash characters in store path"
                else
                  Right
                    StorePath
                      { storePathFull = path,
                        storePathName = name,
                        storePathHash = hashPart
                      }

-- | Extract the relative path after /nix/store/hash-name/
storePathRelative :: StorePath -> FilePath
storePathRelative sp =
  let afterStore = T.drop (T.length nixStore + 1) (storePathFull sp)
      afterName = T.drop (T.length (storePathName sp)) afterStore
      -- drop leading /
      rel = T.dropWhile (== '/') afterName
   in if T.null rel then "" else toS rel

-- | Undo GCC hash mangling. GCC uppercases the hash part of store
-- paths in debug symbols to remove references. This lowercases it back.
demangle :: Text -> Text
demangle path =
  let prefix = T.take (T.length nixStore + 1) path
      rest = T.drop (T.length nixStore + 1) path
      hashPart = T.toLower (T.take hashLen rest)
      afterHash = T.drop hashLen rest
   in prefix <> hashPart <> afterHash

-- | Characters valid in nix base32 hashes.
isNixBase32Char :: Char -> Bool
isNixBase32Char c =
  isDigit c
    || isAsciiLower c
    || c == '-'
    || c == '_'
