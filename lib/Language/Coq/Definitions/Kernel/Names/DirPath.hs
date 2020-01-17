-- | coq/kernel/names.ml module DirPath

module Language.Coq.Definitions.Kernel.Names.DirPath (
  T,
  empty,
  equal,
  ) where

import qualified Language.Coq.Definitions.Kernel.Names.Id as Id

type ModuleIdent = Id.T

type T = [ModuleIdent]

equal :: T -> T -> Bool
equal a b = and $ zipWith Id.equal a b

empty :: T
empty = []
