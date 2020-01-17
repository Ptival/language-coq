-- | coq/kernel/univ.ml module Instance

module Language.Coq.Definitions.Kernel.Univ.Instance (
  T,
  ) where

import qualified Language.Coq.Definitions.Kernel.Univ.Level as Level

type T = [Level.T]
