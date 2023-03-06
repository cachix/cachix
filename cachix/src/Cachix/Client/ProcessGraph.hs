{-# LANGUAGE ScopedTypeVariables #-}

module Cachix.Client.ProcessGraph where

import Control.Concurrent.Async (mapConcurrently)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import qualified Data.Set as Set
import Protolude

processGraph ::
  forall a.
  (Ord a) =>
  -- | Initial list of items
  [a] ->
  -- | Function to process an item and return a list of new items
  (a -> IO [a]) ->
  -- | Set of processed items
  IO (Set a)
processGraph initialItems processItem = do
  processedItems <- newIORef Set.empty
  let processNode :: a -> IO ()
      processNode item = do
        alreadyProcessed <- atomicModifyIORef' processedItems $ \items ->
          if Set.member item items
            then (items, True)
            else (Set.insert item items, False)
        unless alreadyProcessed $ do
          newItems <- processItem item
          _ <- mapConcurrently processNode newItems
          return ()
  _ <- mapConcurrently processNode initialItems
  readIORef processedItems
