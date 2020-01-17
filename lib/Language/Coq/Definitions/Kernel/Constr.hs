-- | coq/kernel/constr.ml

module Language.Coq.Definitions.Kernel.Constr (
  ) where

import qualified Language.Coq.Definitions.Kernel.Evar           as Evar
import qualified Language.Coq.Definitions.Kernel.Names          as Names
import qualified Language.Coq.Definitions.Kernel.Names.Constant as Constant
import qualified Language.Coq.Definitions.Kernel.Names.Id       as Id
import qualified Language.Coq.Definitions.Kernel.Names.Name     as Name
import qualified Language.Coq.Definitions.Kernel.Sorts          as Sorts
import qualified Language.Coq.Definitions.Kernel.Univ.Instance  as Instance
import qualified Language.Coq.Definitions.Kernel.Univ.Universe  as Universe

type ExistentialKey = Evar.T

data CastKind
  = VMCast
  | NativeCast
  | DefaultCast
  | RevertCast

data CaseStyle
  = LetStyle
  | IfStyle
  | LetPatternStyle
  | MatchStyle
  | RegularStyle

data CasePrinting = CasePrinting
  { indTags  :: [Bool]
  , cstrTags :: [[Bool]]
  , style    :: CaseStyle
  }

data CaseInfo = CaseInfo
  { ciInd        :: Names.Inductive
  , ciNPar       :: Int
  , ciCstrNDecls :: [Int]
  , ciCstrNArgs  :: [Int]
  , ciPPInfo     :: CasePrinting
  }

type PExistential c = (ExistentialKey, [c])
type PrecDeclaration c t = ([Name.T], [t], [c])

data KindOfTerm c t s u
  = Rel    Int
  | Var    Id.T
  -- | Meta
  -- | Evar
  -- | Sort
  -- | Cast
  | Prod   Name.T           t   t
  | Lambda Name.T           t   c
  | LetIn  Name.T           c   t c
  | App    c                [c]
  | Const  Constant.T       u
  | Ind    Names.Inductive  u
  -- | Construct
  -- | Case
  -- | Fix
  -- | CoFix
  -- | Proj

data T = T { unT :: KindOfTerm T T Sorts.T Instance.T }

type Constr = T
