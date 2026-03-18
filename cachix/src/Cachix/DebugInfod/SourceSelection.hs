module Cachix.DebugInfod.SourceSelection
  ( findSourceFile,
  )
where

import Cachix.DebugInfod.Vfs (splitComponents)
import Protolude hiding (toS)
import Protolude.Conv
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import System.IO.Error (userError)

-- | Find a source file matching the request in the source directory,
-- with overlay directory support. Overlay files (patched during build)
-- take priority over original source files.
--
-- Uses fuzzy matching: finds all files with the same filename, then
-- picks the one whose path suffix best matches the request.
-- Throws an exception when multiple candidates tie (ambiguous match).
findSourceFile :: FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)
findSourceFile sourceDir overlayDir request = do
  let filename = takeFileName request
  sourceCandidates <- findFilesByName sourceDir filename
  case bestMatch sourceCandidates request of
    Left err -> throwIO $ userError $ toS err
    Right Nothing -> pure Nothing
    Right (Just bestSource) -> do
      overlayCandidates <- findFilesByName overlayDir filename
      let matchingOverlays = filter (overlayMatchesSource sourceCandidates bestSource) overlayCandidates
      case matchingOverlays of
        [overlay] -> pure (Just (overlayDir </> overlay))
        _ -> pure (Just (sourceDir </> bestSource))

-- | Recursively find all files with a given name in a directory.
-- Returns paths relative to the directory.
findFilesByName :: FilePath -> FilePath -> IO [FilePath]
findFilesByName dir filename = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else walkDir dir "" filename

walkDir :: FilePath -> FilePath -> FilePath -> IO [FilePath]
walkDir root relative filename = do
  let absDir = if null relative then root else root </> relative
  entries <- listDirectoryMaybe absDir
  results <- for entries $ \entry -> do
    let absPath = absDir </> entry
        relPath = if null relative then entry else relative </> entry
    isDir <- doesDirectoryExist absPath
    isFile <- doesFileExist absPath
    if isDir
      then walkDir root relPath filename
      else
        if isFile && takeFileName entry == filename
          then pure [relPath]
          else pure []
  pure (concat results)

listDirectoryMaybe :: FilePath -> IO [FilePath]
listDirectoryMaybe dir = do
  result <- try @SomeException $ listDirectory dir
  case result of
    Left _ -> pure []
    Right entries -> pure entries

-- | Score how well a candidate path matches the reference by counting
-- matching path components from the end. Higher is better.
matchingMeasure :: FilePath -> FilePath -> Int
matchingMeasure candidate reference =
  let candidateParts = splitComponents candidate
      referenceParts = splitComponents reference
      matching = length $ takeWhile (uncurry (==)) $ zip (reverse candidateParts) (reverse referenceParts)
   in matching

-- | Find the best matching candidate for a reference path.
-- Returns Left with an error message when multiple candidates tie.
bestMatch :: [FilePath] -> FilePath -> Either Text (Maybe FilePath)
bestMatch [] _ = Right Nothing
bestMatch candidates reference =
  let scored = map (\c -> (matchingMeasure c reference, c)) candidates
      maxScore = maximum (map fst scored)
      bests = filter (\(s, _) -> s == maxScore) scored
   in case bests of
        [(_, path)] -> Right (Just path)
        _ ->
          let ambiguous = map snd bests
           in Left $
                "cannot tell "
                  <> show ambiguous
                  <> " apart for target "
                  <> show reference

-- | Check if an overlay candidate corresponds to the given best source match.
overlayMatchesSource :: [FilePath] -> FilePath -> FilePath -> Bool
overlayMatchesSource sourceCandidates bestSource overlayCandidate =
  case bestMatch sourceCandidates overlayCandidate of
    Right (Just matched) -> matched == bestSource
    _ -> False
