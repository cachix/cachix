-- Deals with adding private caches to netrc
module Cachix.Client.NetRc
  ( add,
  )
where

import Cachix.API.Error (escalateAs)
import Cachix.Client.Exception (CachixException (NetRcParseError))
import Cachix.Types.BinaryCache qualified as BinaryCache
import Data.ByteString qualified as BS
import Data.List (nubBy)
import Data.Text qualified as T
import Network.NetRc
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client (Token, getToken)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

-- | Add a list of binary caches to netrc under `filename`.
--   Makes sure there are no duplicate entries (using domain as a key).
--   If file under filename doesn't exist it's created.
--   The write is skipped when the rendered contents already match the
--   file byte for byte, so repeated runs leave the credentials file
--   untouched while a changed auth token still rewrites it.
--   Returns whether a write happened.
add ::
  Token ->
  [BinaryCache.BinaryCache] ->
  FilePath ->
  IO Bool
add cachixAuthToken binarycaches filename = do
  doesExist <- doesFileExist filename
  existing <-
    if doesExist
      then Just <$> BS.readFile filename
      else return Nothing
  netrc <- maybe (return $ NetRc [] []) parse existing
  let rendered = netRcToByteString $ uniqueAppend netrc
  if Just rendered == existing
    then return False
    else do
      createDirectoryIfMissing True (takeDirectory filename)
      BS.writeFile filename rendered
      return True
  where
    parse :: ByteString -> IO NetRc
    parse contents = escalateAs (NetRcParseError . show) $ parseNetRc filename contents
    -- O(n^2) but who cares?
    uniqueAppend :: NetRc -> NetRc
    uniqueAppend (NetRc hosts macdefs) =
      let f :: NetRcHost -> NetRcHost -> Bool
          f x y = nrhName x == nrhName y
       in NetRc (nubBy f (new ++ hosts)) macdefs
    new :: [NetRcHost]
    new = map mkHost $ filter (not . BinaryCache.isPublic) binarycaches
    mkHost :: BinaryCache.BinaryCache -> NetRcHost
    mkHost bc =
      NetRcHost
        { nrhName = toS $ stripPrefix "http://" $ stripPrefix "https://" (BinaryCache.uri bc),
          nrhLogin = "",
          nrhPassword = getToken cachixAuthToken,
          nrhAccount = "",
          nrhMacros = []
        }
      where
        -- stripPrefix that either strips or returns the same string
        stripPrefix :: Text -> Text -> Text
        stripPrefix prefix str =
          maybe str identity $ T.stripPrefix prefix str
