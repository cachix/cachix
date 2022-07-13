-- Deals with adding private caches to netrc
module Cachix.Client.NetRc
  ( add,
  )
where

import Cachix.API.Error (escalateAs)
import Cachix.Client.Exception (CachixException (NetRcParseError))
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Data.ByteString as BS
import Data.List (nubBy)
import qualified Data.Text as T
import Network.NetRc
import Protolude hiding (toS)
import Protolude.Conv
import Servant.Auth.Client (Token, getToken)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

-- | Add a list of binary caches to netrc under `filename`.
--   Makes sure there are no duplicate entries (using domain as a key).
--   If file under filename doesn't exist it's created.
add ::
  Token ->
  [BinaryCache.BinaryCache] ->
  FilePath ->
  IO ()
add cachixAuthToken binarycaches filename = do
  doesExist <- doesFileExist filename
  netrc <-
    if doesExist
      then BS.readFile filename >>= parse
      else return $ NetRc [] []
  createDirectoryIfMissing True (takeDirectory filename)
  BS.writeFile filename $ netRcToByteString $ uniqueAppend netrc
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
          -- Workaround for bug in libcurl where netrc is not respected
          -- in the absence of the login token.
          nrhLogin = "\"\"",
          nrhPassword = getToken cachixAuthToken,
          nrhAccount = "",
          nrhMacros = []
        }
      where
        -- stripPrefix that either strips or returns the same string
        stripPrefix :: Text -> Text -> Text
        stripPrefix prefix str =
          maybe str identity $ T.stripPrefix prefix str
