-- | coq/kernel/univ.ml module Universe

module Language.Coq.Definitions.Kernel.Univ.Universe (
  T,
  ) where

import qualified Language.Coq.Definitions.Kernel.Univ.Level as Level

type T = (Level.T, Int)
