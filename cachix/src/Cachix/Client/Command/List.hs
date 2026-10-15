module Cachix.Client.Command.List (list, cachixCacheName) where

import Cachix.Client.Config qualified as Config
import Cachix.Client.Env (Env (..))
import Cachix.Client.NixConf qualified as NixConf
import Cachix.Client.OptionsParser (ListOptions (..))
import Cachix.Client.URI (URI)
import Cachix.Client.URI qualified as URI
import Data.List (nub)
import Data.Text qualified as T
import Protolude hiding (list, toS)
import Protolude.Conv (toS)
import URI.ByteString qualified as UBS

data Entry = Entry
  { entrySource :: FilePath,
    entryUri :: Text,
    entryCacheName :: Maybe Text
  }

list :: Env -> ListOptions -> IO ()
list env ListOptions {listAll} = do
  let cachixHost = Config.hostname (config env)
  globalSources <- readSources NixConf.Global
  localSources <- readSources NixConf.Local
  let allSources = globalSources <> localSources
      entries = concatMap (toEntries cachixHost) allSources
      filtered =
        if listAll
          then entries
          else filter (isJust . entryCacheName) entries
  if null filtered
    then
      if listAll
        then putStrLn ("No substituters configured in nix.conf." :: Text)
        else putStrLn ("No Cachix caches configured. Try `cachix use <name>` first." :: Text)
    else printEntries filtered

readSources :: NixConf.NixConfLoc -> IO [NixConf.NixConfSource]
readSources loc =
  NixConf.read loc >>= \case
    Nothing -> pure []
    Just s -> NixConf.resolveIncludes s

toEntries :: URI -> NixConf.NixConfSource -> [Entry]
toEntries cachixHost src =
  [ Entry
      { entrySource = NixConf.nixConfPath src,
        entryUri = u,
        entryCacheName = cachixCacheName cachixHost u
      }
    | u <- nub (fmap normalizeUri (NixConf.readLines NixConf.isSubstituter src))
  ]

normalizeUri :: Text -> Text
normalizeUri = T.dropWhileEnd (== '/')

cachixCacheName :: URI -> Text -> Maybe Text
cachixCacheName cachixHost rawUri = do
  parsed <- rightToMaybe $ URI.parseURI (toS rawUri)
  let cHost = toS (UBS.hostBS (URI.getHostname cachixHost)) :: Text
      sHost = toS (UBS.hostBS (URI.getHostname parsed)) :: Text
      suffix = "." <> cHost
  guard (suffix `T.isSuffixOf` sHost && T.length sHost > T.length suffix)
  pure $ T.dropEnd (T.length suffix) sHost

printEntries :: [Entry] -> IO ()
printEntries entries =
  forM_ grouped $ \(path, es) -> do
    putStrLn $ (toS path :: Text) <> ":"
    forM_ es printEntry
    putStrLn ("" :: Text)
  where
    grouped =
      [ (p, [e | e <- entries, entrySource e == p])
        | p <- nub (fmap entrySource entries)
      ]

    nameWidth = maximum (0 : [T.length n | e <- entries, Just n <- [entryCacheName e]])

    printEntry e =
      case entryCacheName e of
        Just name ->
          putStrLn $ "  " <> T.justifyLeft nameWidth ' ' name <> "  " <> entryUri e
        Nothing ->
          putStrLn $ "  " <> T.justifyLeft nameWidth ' ' "-" <> "  " <> entryUri e
