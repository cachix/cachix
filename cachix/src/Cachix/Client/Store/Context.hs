{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cachix.Client.Store.Context where

import qualified Data.Map as M
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Types as C
import Protolude hiding (Set)

-- | A Nix @ref@
data Ref a

-- | A Nix @PathSet@ aka @std::set<Path>@ aka @std::set<std::string>>@
type PathSet = Set CxxString

data NixStore

data ValidPathInfo

-- | An STL @set@
data Set a

-- | An STL @::iterator@
data Iterator a

-- | An @std::string@
data CxxString

context :: C.Context
context =
  C.cppCtx <> C.fptrCtx <> C.bsCtx
    <> mempty
      { C.ctxTypesTable =
          M.singleton (C.TypeName "refStore") [t|Ref NixStore|]
            <> M.singleton
              (C.TypeName "refValidPathInfo")
              [t|Ref ValidPathInfo|]
            <> M.singleton (C.TypeName "PathSet") [t|PathSet|]
            <> M.singleton (C.TypeName "PathSetIterator") [t|Iterator PathSet|]
      }
