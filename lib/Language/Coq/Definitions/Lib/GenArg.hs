{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- | coq/lib/genarg.ml

module Language.Coq.Definitions.Lib.GenArg (
  RawGenericArgument,
  ) where

import           Data.Text.Prettyprint.Doc                (pretty)
import qualified Language.Coq.Definitions.Lib.GenArg.ArgT as ArgT
import           PrettyPrinter.ToSExp

data RLevel
data GLevel
data TLevel

data GenArgType :: * -> * -> * -> * where
  ExtraArg :: ArgT.Tag   a b c -> GenArgType a         b         c
  ListArg  :: GenArgType a b c -> GenArgType [a]       [b]       [c]
  OptArg   :: GenArgType a b c -> GenArgType (Maybe a) (Maybe b) (Maybe c)
  PairArg  :: GenArgType a1 b1 c1 -> GenArgType a2 b2 c2 -> GenArgType (a1, a2) (b1, b2) (c1, c2)

data AbstractArgumentType :: * -> * -> * where
  RawWit :: GenArgType a b c -> AbstractArgumentType a RLevel
  GlbWit :: GenArgType a b c -> AbstractArgumentType b GLevel
  TopWit :: GenArgType a b c -> AbstractArgumentType c TLevel

data GenericArgument l where
  GenArg :: (AbstractArgumentType a l, a) -> GenericArgument l

instance ToSExp (GenericArgument l) where
  toSExp = const $ pretty "(TODO: Lib.GenArg.GenericArgument)"

type RawGenericArgument = GenericArgument RLevel
