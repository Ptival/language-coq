{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/kernel/declarations.ml

module Language.Coq.Definitions.Kernel.Declarations (
  RecursivityKind(..),
  ) where

import GHC.Generics         (Generic)
import PrettyPrinter.ToSExp (ToSExp)

data RecursivityKind
  = Finite
  | CoFinite
  | BiFinite
  deriving (Generic, ToSExp)
