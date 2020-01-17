{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- | coq/vernac/vernacexpr.ml

module Language.Coq.Definitions.Vernac.VernacExpr (
  ConstructorExpr,
  ConstructorListOrRecordDeclExpr(..),
  InductiveKind(..),
  VernacControl(..),
  VernacCumulative(..),
  VernacExpr(..),
  testVernacControl,
  ) where

import           Data.Text.Prettyprint.Doc                    (hsep, pretty)
import           GHC.Generics                                 (Generic)

import qualified Language.Coq.Definitions.Kernel.Declarations as Declarations
import qualified Language.Coq.Definitions.Kernel.Names        as Names
import qualified Language.Coq.Definitions.Kernel.Names.Id     as Id
import qualified Language.Coq.Definitions.Kernel.Names.Name   as Name
import qualified Language.Coq.Definitions.Interp.ConstrExpr   as ConstrExpr
import qualified Language.Coq.Definitions.Interp.GenRedExpr   as GenRedExpr
import qualified Language.Coq.Definitions.Lib.CAST            as CAST
import qualified Language.Coq.Definitions.Library.DeclKinds   as DeclKinds
import qualified Language.Coq.Definitions.Library.LibNames    as LibNames
import qualified Language.Coq.Definitions.PreTyping.GlobTerm  as GlobTerm
import           PrettyPrinter.ToSExp

type InstanceFlag = Maybe Bool
type ExportFlag   = Bool

type DeclNotation = (Names.LString, ConstrExpr.ConstrExpr, Maybe ConstrExpr.ConstrExpr)

data LocalDeclExpr
  = AssumExpr Names.LName ConstrExpr.ConstrExpr
  | DefExpr   Names.LName ConstrExpr.ConstrExpr (Maybe ConstrExpr.ConstrExpr)
  deriving (Generic, ToSExp)

data InductiveKind
  = InductiveKW
  | CoInductive
  | Variant
  | Record
  | Structure
  | Class       Bool
  deriving (Generic)

instance ToSExp InductiveKind where
  toSExp = \case
    InductiveKW -> pretty "Inductive_kw"
    CoInductive -> pretty "CoInductive"
    Variant     -> pretty "Variant"
    Record      -> pretty "Record"
    Structure   -> pretty "Structure"
    Class b     -> hsep [pretty "Class", toSExp b]

type WithCoercion a = (CoercionFlag, a)
type WithInstance a = (InstanceFlag, a)
type WithNotation a = (a, [DeclNotation])
type WithPriority a = (a, Maybe Int)

type ConstructorExpr = WithCoercion (Names.LIdent, ConstrExpr.ConstrExpr)

data ConstructorListOrRecordDeclExpr
  = Constructors [ConstructorExpr]
  | RecordDecl   (Maybe Names.LIdent) [WithNotation (WithPriority (WithInstance LocalDeclExpr))]
  deriving (Generic, ToSExp)

type InductiveExpr =
  ( WithCoercion ConstrExpr.IdentDecl
  , [ConstrExpr.LocalBinderExpr]
  , Maybe ConstrExpr.ConstrExpr
  , InductiveKind
  , ConstructorListOrRecordDeclExpr
  )

type CoercionFlag = Bool

type InductiveFlag = Declarations.RecursivityKind

data VernacCumulative
  = VernacCumulative
  | VernacNonCumulative
  deriving (Generic, ToSExp)

data DefinitionExpr
  = ProveBody  [ConstrExpr.LocalBinderExpr] ConstrExpr.ConstrExpr
  | DefineBody [ConstrExpr.LocalBinderExpr] (Maybe GenRedExpr.RawRedExpr) ConstrExpr.ConstrExpr (Maybe ConstrExpr.ConstrExpr)
  deriving (Generic, ToSExp)

data VernacExpr
  = VernacDefinition (DeclKinds.Discharge, DeclKinds.DefinitionObjectKind) ConstrExpr.NameDecl DefinitionExpr
  | VernacInductive  (Maybe VernacCumulative) DeclKinds.PrivateFlag InductiveFlag [(InductiveExpr, [DeclNotation])]
  | VernacImport     ExportFlag [LibNames.QualId]
  deriving (Generic, ToSExp)

type VernacFlags = [(String, VernacFlagValue)]

data VernacFlagValue
  = VernacFlagEmpty
  | VernacFlagLeaf String
  | VernacFlagList VernacFlags
  deriving (Generic, ToSExp)

data VernacControl
  = VernacExpr VernacFlags VernacExpr
  -- TODO
  deriving (Generic, ToSExp)

setConstrExpr :: ConstrExpr.ConstrExpr
setConstrExpr = CAST.make Nothing $ ConstrExpr.CSort GlobTerm.GSet

testConstructorExpr :: ConstructorExpr
testConstructorExpr = (False, (CAST.make Nothing (Id.ofString "MyConstructor"), setConstrExpr))

testInductiveExpr :: InductiveExpr
testInductiveExpr =
  ( (False, (CAST.make Nothing (Id.ofString "MyInductive"), Nothing))
  , [ConstrExpr.CLocalAssum
     [CAST.make Nothing $ Name.Name (Id.ofString "myInductiveParameter")]
     (ConstrExpr.Default DeclKinds.Explicit)
     setConstrExpr
    ]
  , Nothing -- Just setConstrExpr
  , InductiveKW
  , Constructors
    [ testConstructorExpr
    ]
  )

testVernacExpr :: VernacExpr
testVernacExpr =
  VernacInductive
  Nothing
  False
  Declarations.Finite
  [ (testInductiveExpr, [])
  ]

testVernacControl :: VernacControl
testVernacControl =
  VernacExpr [] testVernacExpr
