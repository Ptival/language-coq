-- | coq/kernel/univ.ml module RawLevel

module Language.Coq.Definitions.Kernel.Univ.RawLevel (
  T,
  ) where

import qualified Language.Coq.Definitions.Kernel.Names.DirPath as DirPath

data T
  = Prop
  | Set
  | Level Int DirPath.T
  | Var Int
