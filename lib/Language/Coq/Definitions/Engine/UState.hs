{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/engine/uState.ml

module Language.Coq.Definitions.Engine.UState (
  GenUniverseDecl,
  ) where

import GHC.Generics         (Generic)

import PrettyPrinter.ToSExp (ToSExp)

data GenUniverseDecl a b = GenUniverseDecl
  { univDeclInstance              :: a
  , univDeclExtensibleInstance    :: Bool
  , univDeclConstraints           :: b
  , univDeclExtensibleConstraints :: Bool
  }
  deriving (Generic, ToSExp)
