-- | coq/kernel/names.ml

module Language.Coq.Definitions.Kernel.Names (
  module DirPath,
  Inductive,
  LIdent,
  LName,
  LString,
  ) where

import qualified Language.Coq.Definitions.Lib.CAST             as CAST
import           Language.Coq.Definitions.Kernel.Names.DirPath as DirPath
import qualified Language.Coq.Definitions.Kernel.Names.Id      as Id
import qualified Language.Coq.Definitions.Kernel.Names.Name    as Name
import qualified Language.Coq.Definitions.Kernel.Names.MutInd  as MutInd
import           PrettyPrinter.ToSExp

type Inductive = (MutInd.T, Int)
type Constructor = (Inductive, Int)

type LIdent  = CAST.T Id.T
type LName   = CAST.T Name.T
type LString = CAST.T String
