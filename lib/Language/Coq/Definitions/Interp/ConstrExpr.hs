{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/interp/constrexpr.ml

module Language.Coq.Definitions.Interp.ConstrExpr (
  BinderKind(..),
  CasesPatternExprR,
  ConstrExpr,
  ConstrExprR(..),
  ConstrPatternExpr,
  IdentDecl,
  LocalBinderExpr(..),
  NameDecl,
  OrByNotation,
  ) where

import           GHC.Generics                                (Generic)

import qualified Language.Coq.Definitions.Engine.EVarKinds   as EVarKinds
import qualified Language.Coq.Definitions.Engine.NameGen     as NameGen
import qualified Language.Coq.Definitions.Engine.UState      as UState
import qualified Language.Coq.Definitions.Lib.CAST           as CAST
import qualified Language.Coq.Definitions.Lib.GenArg         as GenArg
import qualified Language.Coq.Definitions.Library.DeclKinds  as DeclKinds
import qualified Language.Coq.Definitions.Library.LibNames   as LibNames
import qualified Language.Coq.Definitions.Kernel.Names       as Names
import qualified Language.Coq.Definitions.Kernel.Names.Id    as Id
import qualified Language.Coq.Definitions.PreTyping.GlobTerm as GlobTerm
import           PrettyPrinter.ToSExp                        (ToSExp)

type UniverseDeclExpr = UState.GenUniverseDecl [Names.LIdent] [GlobTerm.GlobConstraint]

type IdentDecl = (Names.LIdent, Maybe UniverseDeclExpr)
type NameDecl  = (Names.LName,  Maybe UniverseDeclExpr)

data OrByNotationR a
  = AN         a
  | ByNotation String (Maybe String)
  deriving (Generic, ToSExp)

type OrByNotation a = CAST.T (OrByNotationR a)

data Explicitation
  = ExplByPos  Int (Maybe Id.T)
  | ExplByName Id.T
  deriving (Generic, ToSExp)

data BinderKind
  = Default     DeclKinds.BindingKind
  | Generalized DeclKinds.BindingKind DeclKinds.BindingKind Bool
  deriving (Generic, ToSExp)

data AbstractionKind
  = AbsLambda
  | AbsPi
  deriving (Generic, ToSExp)

type ProjFlag = Maybe Int

type Sign = Bool
type RawNaturalNumber = String

data PrimToken
  = Numeral RawNaturalNumber Sign
  | String  String
  deriving (Generic, ToSExp)

data CasesPatternExprR
  = CPatAlias CasesPatternExpr Names.LName
  -- TODO
  deriving (Generic, ToSExp)

type CasesPatternExpr = CAST.T CasesPatternExprR

data ConstrExprR
  -- | CRef
  = CFix     Names.LIdent [FixExpr]
  -- | CCoFix
  | CProdN   [LocalBinderExpr] ConstrExpr
  | CLambdaN [LocalBinderExpr] ConstrExpr
  | CLetIn   Names.LName ConstrExpr (Maybe ConstrExpr) ConstrExpr
  -- | CAppExpl
  | CApp     (ProjFlag, ConstrExpr) [(ConstrExpr, Maybe (CAST.T Explicitation))]
  -- | CRecord
  -- | CCases
  -- | CLetTuple
  | CIf      ConstrExpr (Maybe Names.LName, Maybe ConstrExpr) ConstrExpr ConstrExpr
  | CHole    (Maybe EVarKinds.T) (NameGen.IntroPatternNamingExpr) (Maybe GenArg.RawGenericArgument)
  -- | CPatVar
  -- | CEVar
  | CSort    GlobTerm.GlobSort
  -- | CCast
  -- | CNotation
  -- | CGeneralization
  | CPrim    PrimToken
  -- | CDelimiters
  deriving (Generic, ToSExp)

type ConstrExpr = CAST.T ConstrExprR

type FixExpr =
  ( Names.LIdent
  , (Maybe Names.LIdent, RecursionOrderExpr)
  , [LocalBinderExpr]
  , ConstrExpr
  , ConstrExpr
  )

data RecursionOrderExpr
  = CStructRec
  | CWfRec      ConstrExpr
  | CMeasureRec ConstrExpr (Maybe ConstrExpr)
  deriving (Generic, ToSExp)

data LocalBinderExpr
  = CLocalAssum   [Names.LName] BinderKind ConstrExpr
  | CLocalDef     Names.LName ConstrExpr (Maybe ConstrExpr)
  | CLocalPattern (CAST.T (CasesPatternExpr, Maybe ConstrExpr))
  deriving (Generic, ToSExp)

type ConstrPatternExpr = ConstrExpr

data WithDeclarationAST
  = CWithModule     (CAST.T [Id.T]) LibNames.QualId
  | CWithDefinition (CAST.T [Id.T]) (Maybe UniverseDeclExpr) ConstrExpr
  deriving (Generic, ToSExp)

data ModuleASTR
  = CMIdent LibNames.QualId
  | CMApply ModuleAST ModuleAST
  | CMWith  ModuleAST WithDeclarationAST
  deriving (Generic, ToSExp)

type ModuleAST = CAST.T ModuleASTR
