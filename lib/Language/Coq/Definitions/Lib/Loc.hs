{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/lib/loc.ml

module Language.Coq.Definitions.Lib.Loc (
  Source(..),
  T(..),
  ) where

import GHC.Generics         (Generic)

import PrettyPrinter.ToSExp (ToSExp)

data Source
  = InFile        String
  | TopLevelInput
  deriving (Generic, ToSExp)

data T = T
  { fName      :: Source
  , lineNb     :: Int
  , bolPos     :: Int
  , lineNbLast :: Int
  , bolPosLast :: Int
  , bp         :: Int
  , ep         :: Int
  }
  deriving (Generic, ToSExp)
