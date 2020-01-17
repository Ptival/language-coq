-- | coq/lib/genarg.ml module ArgT

module Language.Coq.Definitions.Lib.GenArg.ArgT (
  Tag,
  ) where

import qualified Language.Coq.Definitions.CLib.Dyn.Make as Make

type Tag a b c = Make.Tag (a, b, c)
