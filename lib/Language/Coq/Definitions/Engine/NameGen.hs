{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/engine/namegen.ml

module Language.Coq.Definitions.Engine.NameGen (
  IntroPatternNamingExpr(..),
  ) where

import           GHC.Generics                             (Generic)

import qualified Language.Coq.Definitions.Kernel.Names.Id as Id
import           PrettyPrinter.ToSExp                     (ToSExp)

data IntroPatternNamingExpr
  = IntroIdentifier Id.T
  | IntroFresh      Id.T
  | IntroAnonymous
  deriving (Generic, ToSExp)
