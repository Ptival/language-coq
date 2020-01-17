-- | coq/kernel/univ.ml module Level

module Language.Coq.Definitions.Kernel.Univ.Level (
  T,
  ) where

import qualified Language.Coq.Definitions.Kernel.Univ.RawLevel as RawLevel

data T = T
  { hash  :: Int
  , data' :: RawLevel.T
  }
