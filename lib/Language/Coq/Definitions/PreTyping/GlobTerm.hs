{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/pretyping/glob_term.ml

module Language.Coq.Definitions.PreTyping.GlobTerm (
  GlobConstraint,
  GlobSort,
  GlobSortGen(..),
  LevelInfo,
  UniverseKind(..),
  ) where

import           GHC.Generics                              (Generic)

import qualified Language.Coq.Definitions.Kernel.Univ      as Univ
import qualified Language.Coq.Definitions.Library.LibNames as LibNames
import           PrettyPrinter.ToSExp                      (ToSExp)

data GlobSortGen a
  = GProp
  | GSet
  | GType a
  deriving (Generic, ToSExp)

data UniverseKind a
  = UAnonymous
  | UUnknown
  | UNamed a
  deriving (Generic, ToSExp)

type LevelInfo = UniverseKind LibNames.QualId
type GlobLevel = GlobSortGen LevelInfo
type GlobConstraint = (GlobLevel, Univ.ConstraintType, GlobLevel)

type SortInfo = [Maybe (LibNames.QualId, Int)]
type GlobSort = GlobSortGen SortInfo
