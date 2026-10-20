module Cachix.Client.Command.List (list, cachixCacheName, Entry (..), configuredEntries) where

import Cachix.Client.Env (Env, effectiveHost)
import Cachix.Client.Exception (CachixException)
import Cachix.Client.NixConf qualified as NixConf
import Cachix.Client.OptionsParser (ListOptions (..))
import Cachix.Client.URI (URI)
import Cachix.Client.URI qualified as URI
import Control.Exception.Safe qualified as Safe
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Protolude hiding (hPutStrLn, list, toS)
import Protolude.Conv (toS)
import System.IO (hPutStrLn)

data Entry = Entry
  { entrySource :: FilePath,
    entryUri :: Text,
    entryKey :: Text,
    entryCacheName :: Maybe Text
  }

list :: Env -> ListOptions -> IO ()
list env ListOptions {listAll} = do
  let cachixHost = effectiveHost env
  globalSources <- readSources NixConf.Global
  localSources <- readSources NixConf.Local
  let filtered = configuredEntries cachixHost ListOptions {listAll} (globalSources <> localSources)
  if null filtered
    then
      if listAll
        then putStrLn ("No substituters configured in nix.conf." :: Text)
        else putStrLn ("No Cachix caches configured. Try `cachix use <name>` first." :: Text)
    else printEntries filtered

-- | Select and deduplicate entries before rendering, retaining their source.
configuredEntries :: URI -> ListOptions -> [NixConf.NixConfSource] -> [Entry]
configuredEntries cachixHost ListOptions {listAll} sources =
  let allSources = dedupBy NixConf.nixConfPath sources
      entries = dedupBy entryKey (concatMap (toEntries cachixHost) allSources)
   in if listAll then entries else filter (isJust . entryCacheName) entries

readSources :: NixConf.NixConfLoc -> IO [NixConf.NixConfSource]
readSources loc =
  NixConf.read loc >>= \case
    Nothing -> pure []
    Just s ->
      Safe.catch (NixConf.resolveIncludes s) $ \(e :: CachixException) -> do
        hPutStrLn stderr $
          "Warning: skipping includes from "
            <> NixConf.nixConfPath s
            <> ": "
            <> show e
        pure [s]

toEntries :: URI -> NixConf.NixConfSource -> [Entry]
toEntries cachixHost src =
  [ Entry
      { entrySource = NixConf.nixConfPath src,
        entryUri = u,
        entryKey = canonicalKey u,
        entryCacheName = cachixCacheName cachixHost u
      }
  | u <- NixConf.readLines NixConf.isAnySubstituter src
  ]

-- | Keep the first occurrence of each @key x@, preserving input order.
dedupBy :: (Ord k) => (a -> k) -> [a] -> [a]
dedupBy key = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | Set.member (key x) seen = go seen xs
      | otherwise = x : go (Set.insert (key x) seen) xs

-- | Canonical form used only for deduplication (lowercased scheme + host,
-- default ports dropped). The original
-- substituter text is preserved for display.
canonicalKey :: Text -> Text
canonicalKey raw =
  case URI.parseURI (toS raw) of
    Right parsed | Just k <- URI.canonicalForm parsed -> k
    _ -> raw

cachixCacheName :: URI -> Text -> Maybe Text
cachixCacheName cachixHost rawUri = do
  parsed <- rightToMaybe $ URI.parseURI (toS rawUri)
  cHost <- hostText cachixHost
  sHost <- hostText parsed
  let suffix = "." <> cHost
  guard (suffix `T.isSuffixOf` sHost && T.length sHost > T.length suffix)
  pure $ T.dropEnd (T.length suffix) sHost
  where
    hostText u = T.toLower . toS . URI.hostBS <$> URI.getHostnameMay u

printEntries :: [Entry] -> IO ()
printEntries entries =
  sequence_ $ intersperse (putStrLn ("" :: Text)) (map printGroup grouped)
  where
    grouped :: [(FilePath, [Entry])]
    grouped =
      let bucket =
            foldl'
              (\m e -> Map.insertWith (flip (<>)) (entrySource e) [e] m)
              Map.empty
              entries
          order = dedupBy identity (map entrySource entries)
       in [(p, Map.findWithDefault [] p bucket) | p <- order]

    nameWidth = maximum (0 : [T.length n | e <- entries, Just n <- [entryCacheName e]])

    printGroup (path, es) = do
      putStrLn $ (toS path :: Text) <> ":"
      forM_ es printEntry

    printEntry e =
      case entryCacheName e of
        Just name ->
          putStrLn $ "  " <> T.justifyLeft nameWidth ' ' name <> "  " <> entryUri e
        Nothing ->
          putStrLn $ "  " <> T.justifyLeft nameWidth ' ' "-" <> "  " <> entryUri e
