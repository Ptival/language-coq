{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/interp/genredexpr.ml

module Language.Coq.Definitions.Interp.GenRedExpr (
  RawRedExpr,
  ) where

import           GHC.Generics                               (Generic)

import qualified Language.Coq.Definitions.Interp.ConstrExpr as ConstrExpr
import qualified Language.Coq.Definitions.Library.LibNames  as LibNames
import           PrettyPrinter.ToSExp                       (ToSExp)

data RedExprGen a b c
  = Red Bool
  | HNF
  -- TODO
  deriving (Generic, ToSExp)

type RTrm = ConstrExpr.ConstrExpr
type RPat = ConstrExpr.ConstrPatternExpr
type RCst = ConstrExpr.OrByNotation LibNames.QualId

type RawRedExpr = RedExprGen RTrm RCst RPat
