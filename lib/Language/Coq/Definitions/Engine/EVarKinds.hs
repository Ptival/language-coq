{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/engine/evar_kinds.ml

module Language.Coq.Definitions.Engine.EVarKinds (
  T(..),
  ) where

import GHC.Generics         (Generic)
import PrettyPrinter.ToSExp (ToSExp)

data T
  = InternalHole
  -- .. TODO
  deriving (Generic, ToSExp)
