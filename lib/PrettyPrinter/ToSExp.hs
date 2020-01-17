{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | `toSExp` turns a value into a s-expression

module PrettyPrinter.ToSExp (
  -- GenericToSExp(..),
  GToSExp(..),
  ToSExp(..),
  ) where

import Data.Text.Prettyprint.Doc (Doc, dquotes, hsep, parens, pretty)
import GHC.Generics

class GToSExp f where
  gToSExp :: f a -> Doc ()

class GToSExpFields f where
  gToSExpFields :: f a -> [Doc ()]

class ToSExp a where
  toSExp :: a -> Doc ()

  default
    toSExp :: Generic a => GToSExp (Rep a) => a -> Doc ()
  toSExp = gToSExp . from

-- `Generic` instances

-- Need a wrapper to trigger generic resolution without forcing all
-- `ToSExp`-able types to also be `Generic`.
-- newtype GenericToSExp a = GenericToSExp { unGenericToSExp :: a }

-- instance (Generic a, GToSExp (Rep a)) => ToSExp (GenericToSExp a) where
--   toSExp = gToSExp . from . unGenericToSExp

instance (GToSExp f, GToSExp g) => GToSExp (f :+: g) where
  gToSExp (L1 a) = gToSExp a
  gToSExp (R1 b) = gToSExp b

instance (GToSExp f, GToSExp g) => GToSExp (f :*: g) where
  gToSExp (a :*: b) = hsep [parens $ gToSExp a, parens $ gToSExp b]

instance ToSExp c => GToSExp (K1 i c) where
  gToSExp = toSExp . unK1

-- instance (Datatype c) => GToSExp (D1 c f) where
--   gToSExp s = pretty ("TODO D: " ++ datatypeName s)

-- instance (Selector c) => GToSExp (S1 c f) where
--   gToSExp s = pretty ("TODO S: " ++ selName s)

isTupleConstructor :: String -> Bool
isTupleConstructor s =
  case s of
  '(' : rest -> dropWhile (== ',') rest == ")"
  _          -> False

instance (Constructor c, GToSExpFields f) => GToSExp (C1 c f) where
  gToSExp c | isTupleConstructor (conName c) =
              parens $ hsep $ gToSExpFields (unM1 c)
            | conIsRecord c =
              case gToSExpFields (unM1 c) of
              [] -> error "TODO: Empty record?"
              fs -> parens $ hsep $ fs
            | otherwise =
              case gToSExpFields (unM1 c) of
              [] -> pretty (conName c)
              fs -> parens $ hsep $ pretty (conName c) : fs

instance GToSExp f => GToSExp (M1 i c f) where
  gToSExp = gToSExp . unM1

-- instance GToSExp U1 where
--   gToSExp = const $ pretty "U1"

instance (GToSExpFields f, GToSExpFields g) => GToSExpFields (f :*: g) where
  gToSExpFields (f :*: g) = gToSExpFields f ++ gToSExpFields g

instance (GToSExp f, Selector c) => GToSExpFields (S1 c f) where
  gToSExpFields s =
    case selName s of
    "" -> (: []) . gToSExp . unM1 $ s
    n  -> (: []) . parens . hsep $ [pretty n, gToSExp (unM1 s)]

-- instance (GToSExp f) => GToSExpFields (M1 i c f) where
--   gToSExpFields = (: []) . gToSExp . unM1

instance GToSExpFields U1 where
  gToSExpFields = const []

-- -- `Prelude` instances

instance ToSExp a => ToSExp [a] where
  toSExp = parens . hsep . map toSExp

instance {-# OVERLAPPING #-} ToSExp String where
  toSExp s = dquotes (pretty s)

instance ToSExp a => ToSExp (Maybe a) where
  toSExp = \case
    Nothing -> pretty "()"
    Just a  -> parens $ toSExp a

instance ToSExp Bool where
  toSExp False = pretty "false"
  toSExp True  = pretty "true"

instance ToSExp Int where
  toSExp = pretty

instance (ToSExp a, ToSExp b) => ToSExp (a, b) where

instance (ToSExp a, ToSExp b, ToSExp c) => ToSExp (a, b, c) where

instance (ToSExp a, ToSExp b, ToSExp c, ToSExp d) => ToSExp (a, b, c, d) where

instance (ToSExp a, ToSExp b, ToSExp c, ToSExp d, ToSExp e) => ToSExp (a, b, c, d, e) where
