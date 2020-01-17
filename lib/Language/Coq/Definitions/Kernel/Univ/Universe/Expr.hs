-- | coq/kernel/univ.ml module Universe.Expr

module Language.Coq.Definitions.Kernel.Univ.Universe.Expr (
  ) where

import qualified Language.Coq.Definitions.Kernel.Univ.Level as Level

type T = (Level.T, Int)
