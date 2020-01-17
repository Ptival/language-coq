{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/library/decl_kinds.ml

module Language.Coq.Definitions.Library.DeclKinds (
  BindingKind(..),
  DefinitionObjectKind,
  Discharge(..),
  PrivateFlag,
  ) where

import GHC.Generics         (Generic)

import PrettyPrinter.ToSExp (ToSExp)

data Discharge
  = DoDischarge
  | NoDischarge
  deriving (Generic, ToSExp)

data Locality
  = Discharge
  | Local
  | Global
  deriving (Generic, ToSExp)

data BindingKind
  = Explicit
  | Implicit
  deriving (Generic, ToSExp)

type Polymorphic = Bool

type PrivateFlag = Bool

type CumulativeInductiveFlag = Bool

data TheoremKind
  = Theorem
  | Lemma
  | Fact
  | Remark
  | Property
  | Proposition
  | Corollary
  deriving (Generic, ToSExp)

data DefinitionObjectKind
  = Definition
  | Coercion
  | SubClass
  | CanonicalStructure
  | Example
  | Fixpoint
  | CoFixpoint
  | Scheme
  | StructureComponent
  | IdentityCoercion
  | Instance
  | Method
  | Let
  deriving (Generic, ToSExp)

data AssumptionObjectKind
  = Definitional
  | Logical
  | Conjectural
  deriving (Generic, ToSExp)

type AssumptionKind = (Locality, Polymorphic, AssumptionObjectKind)

type DefinitionKind = (Locality, Polymorphic, DefinitionObjectKind)

data GoalObjectKind
  = DefinitionBody DefinitionObjectKind
  | Proof          TheoremKind
  deriving (Generic, ToSExp)

type GoalKind = (Locality, Polymorphic, GoalObjectKind)

data LogicalKind
  = IsAssumption AssumptionObjectKind
  | IsDefinition DefinitionObjectKind
  | IsProof      TheoremKind
  deriving (Generic, ToSExp)
