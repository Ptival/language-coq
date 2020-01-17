{-# LANGUAGE GADTs #-}

-- | coq/clib/dyn.ml module Make

module Language.Coq.Definitions.CLib.Dyn.Make (
  Tag,
  ) where

type Tag a = Int

data T where
  Dyn :: (Tag a, a) -> T
