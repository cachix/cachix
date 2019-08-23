{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cachix.Client.Store
  ( Store,
    -- * Getting a Store
    openStore,
    releaseStore,
    -- * Query a path
    followLinksToStorePath,
    queryPathInfo,
    -- * Get closures
    computeFSClosure,
    ClosureParams (..),
    defaultClosureParams,
    PathSet,
    newEmptyPathSet,
    addToPathSet,
    traversePathSet,
    -- * Miscellaneous
    storeUri
    )
where

import Cachix.Client.Store.Context (NixStore, Ref, ValidPathInfo, context)
import qualified Cachix.Client.Store.Context as C hiding (context)
import Data.ByteString.Unsafe (unsafePackMallocCString)
import Data.Coerce
import Foreign.ForeignPtr
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exceptions as C
import Protolude
import System.IO.Unsafe (unsafePerformIO)

C.context context

C.include "<cstring>"

C.include "<nix/config.h>"

C.include "<nix/shared.hh>"

C.include "<nix/store-api.hh>"

C.include "<nix/derivations.hh>"

C.include "<nix/affinity.hh>"

C.include "<nix/globals.hh>"

C.include "aliases.h"

C.using "namespace nix"

-- | TODO: foreignptr
newtype Store = Store (Ptr (Ref NixStore))

openStore :: IO Store
openStore =
  coerce
    [C.throwBlock| refStore* {
      refStore s = openStore();
      return new refStore(s);
    } |]

releaseStore :: Store -> IO ()
releaseStore (Store store) = [C.exp| void { delete $(refStore* store) } |]

-- | Follow symlinks to the store and chop off the parts after the top-level store name
followLinksToStorePath :: Store -> ByteString -> IO ByteString
followLinksToStorePath (Store store) bs =
  unsafePackMallocCString
    =<< [C.throwBlock| const char *{
    return strdup((*$(refStore* store))->followLinksToStorePath(std::string($bs-ptr:bs, $bs-len:bs)).c_str());
  }|]

storeUri :: Store -> IO ByteString
storeUri (Store store) =
  unsafePackMallocCString
    =<< [C.throwBlock| const char* {
             std::string uri = (*$(refStore* store))->getUri();
             return strdup(uri.c_str());
           } |]

-- TODO: test queryPathInfo
queryPathInfo :: Store -> ByteString -> IO (ForeignPtr (Ref ValidPathInfo))
queryPathInfo (Store store) path = do
  vpi <-
    [C.exp| refValidPathInfo* {
             new refValidPathInfo((*$(refStore* store))->queryPathInfo($bs-cstr:path))
           } |]
  newForeignPtr finalizeRefValidPathInfo vpi

finalizeRefValidPathInfo :: FinalizerPtr (Ref ValidPathInfo)

{-# NOINLINE finalizeRefValidPathInfo #-}
finalizeRefValidPathInfo =
  unsafePerformIO
    [C.exp|
  void (*)(refValidPathInfo *) {
    [](refValidPathInfo *v){ delete v; }
  } |]

----- PathSet -----
newtype PathSet = PathSet (ForeignPtr (C.Set C.CxxString))

finalizePathSet :: FinalizerPtr C.PathSet

{-# NOINLINE finalizePathSet #-}
finalizePathSet =
  unsafePerformIO
    [C.exp|
  void (*)(PathSet *) {
    [](PathSet *v){
      delete v;
    }
  } |]

newEmptyPathSet :: IO PathSet
newEmptyPathSet = do
  ptr <- [C.exp| PathSet *{ new PathSet() }|]
  fptr <- newForeignPtr finalizePathSet ptr
  pure $ PathSet fptr

addToPathSet :: ByteString -> PathSet -> IO ()
addToPathSet bs pathSet_ = withPathSet pathSet_ $ \pathSet ->
  [C.throwBlock| void { 
    $(PathSet *pathSet)->insert(std::string($bs-ptr:bs, $bs-len:bs));
  }|]

withPathSet :: PathSet -> (Ptr C.PathSet -> IO b) -> IO b
withPathSet (PathSet pathSetFptr) = withForeignPtr pathSetFptr

traversePathSet :: forall a. (ByteString -> IO a) -> PathSet -> IO [a]
traversePathSet f pathSet_ = withPathSet pathSet_ $ \pathSet -> do
  i <- [C.exp| PathSetIterator *{ new PathSetIterator($(PathSet *pathSet)->begin()) }|]
  end <- [C.exp| PathSetIterator *{ new PathSetIterator ($(PathSet *pathSet)->end()) }|]
  let cleanup =
        [C.throwBlock| void {
          delete $(PathSetIterator *i);
          delete $(PathSetIterator *end);
        }|]
  flip finally cleanup
    $ let go :: ([a] -> [a]) -> IO [a]
          go acc = do
            isDone <-
              [C.exp| int {
            *$(PathSetIterator *i) == *$(PathSetIterator *end)
          }|]
            if isDone /= 0
              then pure $ acc []
              else
                do
                  somePath <- unsafePackMallocCString =<< [C.exp| const char *{ strdup((*$(PathSetIterator *i))->c_str()) } |]
                  a <- f somePath
                  [C.throwBlock| void { (*$(PathSetIterator *i))++; } |]
                  go (acc . (a :))
     in go identity

----- computeFSClosure -----
data ClosureParams
  = ClosureParams
      { flipDirection :: Bool,
        includeOutputs :: Bool,
        includeDerivers :: Bool
        }

defaultClosureParams :: ClosureParams
defaultClosureParams = ClosureParams
  { flipDirection = False,
    includeOutputs = False,
    includeDerivers = False
    }

computeFSClosure :: Store -> ClosureParams -> PathSet -> IO PathSet
computeFSClosure (Store store) params startingSet_ = withPathSet startingSet_ $ \startingSet -> do
  let countTrue :: Bool -> C.CInt
      countTrue True = 1
      countTrue False = 0
      flipDir = countTrue $ flipDirection params
      inclOut = countTrue $ includeOutputs params
      inclDrv = countTrue $ includeDerivers params
  ps <-
    [C.throwBlock| PathSet* {
             PathSet *r = new PathSet();
             (*$(refStore* store))->computeFSClosure(*$(PathSet *startingSet), *r, $(int flipDir), $(int inclOut), $(int inclDrv));
             return r;
           } |]
  fp <- newForeignPtr finalizePathSet ps
  pure $ PathSet fp
