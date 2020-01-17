{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | coq/lib/cAst.ml

module Language.Coq.Definitions.Lib.CAST (
  T(..),
  make,
  withVal,
  ) where

import           GHC.Generics                     (Generic)

import qualified Language.Coq.Definitions.Lib.Loc as Loc
import           PrettyPrinter.ToSExp             (ToSExp)

data T a = T
  { v   :: a
  , loc :: Maybe Loc.T
  }
  deriving (Generic, ToSExp)

make :: Maybe Loc.T -> a -> T a
make loc v = T { v, loc }

withVal :: (a -> b) -> T a -> b
withVal f = f . v
