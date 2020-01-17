-- | coq/kernel/names.ml module ModPath

module Language.Coq.Definitions.Kernel.Names.ModPath (
  T(..),
  equal,
  ) where

import qualified Language.Coq.Definitions.Kernel.Names.DirPath as DirPath
import qualified Language.Coq.Definitions.Kernel.Names.Label   as Label
import qualified Language.Coq.Definitions.Kernel.Names.MBId    as MBId

data T
  = MPFile DirPath.T
  | MPBound MBId.T
  | MPDot T (Label.T)
  deriving (Eq)

equal :: T -> T -> Bool
equal mp1 mp2 = mp1 == mp2 || case (mp1, mp2) of
  (MPFile p1,     MPFile p2)     -> DirPath.equal p1 p2
  (MPBound id1,   MPBound id2)   -> MBId.equal id1 id2
  (MPDot mp1' l1, MPDot mp2' l2) -> l1 == l2 && equal mp1' mp2'
  (MPFile _,      MPBound _)     -> False
  (MPFile _,      MPDot _ _)     -> False
  (MPBound _,     MPFile _)      -> False
  (MPBound _,     MPDot _ _)     -> False
  (MPDot _ _,     MPFile _)      -> False
  (MPDot _ _,     MPBound _)     -> False
