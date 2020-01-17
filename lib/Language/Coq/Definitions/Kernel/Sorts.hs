-- | coq/kernel/sorts.ml

module Language.Coq.Definitions.Kernel.Sorts (
  T,
  ) where

import qualified Language.Coq.Definitions.Kernel.Univ.Universe as Universe

data Family
  = InProp
  | InSet
  | InType

data T
  = Prop
  | Set
  | Type Universe.T
