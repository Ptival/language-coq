{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/kernel/univ.ml

module Language.Coq.Definitions.Kernel.Univ (
  ConstraintType,
) where

import GHC.Generics         (Generic)
import PrettyPrinter.ToSExp (ToSExp)

data ConstraintType
  = Lt
  | Le
  | Eq
  deriving (Generic, ToSExp)
