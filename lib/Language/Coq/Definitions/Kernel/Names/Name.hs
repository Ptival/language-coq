{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/kernel/names.ml module Name

module Language.Coq.Definitions.Kernel.Names.Name (
  T(..),
  ) where

import           GHC.Generics                             (Generic)

import qualified Language.Coq.Definitions.Kernel.Names.Id as Id

import           PrettyPrinter.ToSExp                     (ToSExp)

data T
  = Anonymous
  | Name Id.T
  deriving (Generic, ToSExp)
