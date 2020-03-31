-- Deals with adding private caches to netrc
module Cachix.Client.NetRc
  ( add,
  )
where

import Cachix.API.Error (escalateAs)
import Cachix.Client.Config (Config)
import qualified Cachix.Client.Config as Config
import Cachix.Client.Exception (CachixException (NetRcParseError))
import qualified Cachix.Types.BinaryCache as BinaryCache
import qualified Data.ByteString as BS
import Data.List (nubBy)
import qualified Data.Text as T
import Network.NetRc
import Protolude
import Servant.Auth.Client (Token, getToken)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

-- | Add a list of binary caches to netrc under `filename`.
--   Makes sure there are no duplicate entries (using domain as a key).
--   If file under filename doesn't exist it's created.
add ::
  Config ->
  [BinaryCache.BinaryCache] ->
  FilePath ->
  IO ()
add config binarycaches filename = do
  doesExist <- doesFileExist filename
  cachixAuthToken <- Config.getAuthTokenRequired (Just config)
  netrc <-
    if doesExist
      then BS.readFile filename >>= parse
      else return $ NetRc [] []
  createDirectoryIfMissing True (takeDirectory filename)
  BS.writeFile filename $ netRcToByteString $ uniqueAppend cachixAuthToken netrc
  where
    parse :: ByteString -> IO NetRc
    parse contents = escalateAs (NetRcParseError . show) $ parseNetRc filename contents
    -- O(n^2) but who cares?
    uniqueAppend :: Token -> NetRc -> NetRc
    uniqueAppend cachixAuthToken (NetRc hosts macdefs) =
      let f :: NetRcHost -> NetRcHost -> Bool
          f x y = nrhName x == nrhName y
       in NetRc (nubBy f (new cachixAuthToken ++ hosts)) macdefs
    new :: Token -> [NetRcHost]
    new cachixAuthToken = map (mkHost cachixAuthToken) $ filter (not . BinaryCache.isPublic) binarycaches
    mkHost :: Token -> BinaryCache.BinaryCache -> NetRcHost
    mkHost cachixAuthToken bc =
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
