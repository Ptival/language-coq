-- | coq/kernel/names.ml module KerPair

module Language.Coq.Definitions.Kernel.Names.KerPair (
  T,
  make,
  make1,
  ) where

import qualified Language.Coq.Definitions.Kernel.Names.KerName as KerName

data T
  = Same KerName.T
  | Dual KerName.T KerName.T

same :: KerName.T -> T
same = Same

make :: KerName.T -> KerName.T -> T
make knu knc =
  if KerName.equal knu knc
  then Same knc
  else Dual knu knc

make1 :: KerName.T -> T
make1 = same
