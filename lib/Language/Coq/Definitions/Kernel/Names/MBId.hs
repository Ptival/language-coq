-- | coq/kernel/names.ml module MBId

module Language.Coq.Definitions.Kernel.Names.MBId (
  T,
  equal,
  ) where

import qualified Language.Coq.Definitions.Kernel.Names.DirPath as DirPath
import qualified Language.Coq.Definitions.Kernel.Names.Id      as Id

type T = (Int, Id.T, DirPath.T)

equal :: Eq a => (a, Id.T, DirPath.T) -> (a, Id.T, DirPath.T) -> Bool
equal x y =
  x == y ||
  let (i1, id1, p1) = x in
  let (i2, id2, p2) = y in
  i1 == i2 && Id.equal id1 id2 && DirPath.equal p1 p2
