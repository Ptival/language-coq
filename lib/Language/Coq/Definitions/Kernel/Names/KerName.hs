{-# LANGUAGE NamedFieldPuns #-}

-- | coq/kernel/names.ml module KerName

module Language.Coq.Definitions.Kernel.Names.KerName (
  T,
  equal,
  make,
  make2,
  ) where

import qualified Language.Coq.Definitions.Kernel.Names.DirPath as DirPath
import qualified Language.Coq.Definitions.Kernel.Names.Label   as Label
import qualified Language.Coq.Definitions.Kernel.Names.ModPath as ModPath

data T = T
  { modPath :: ModPath.T
  , dirPath :: DirPath.T
  , knLabel :: Label.T
  -- Not sure this will be necessary, and it's mutable, so skipping it
  -- refHash :: Int
  }

make :: ModPath.T -> DirPath.T -> Label.T -> T
make modPath dirPath knLabel = T
  { modPath
  , dirPath
  , knLabel
  }

make2 :: ModPath.T -> Label.T -> T
make2 modPath knLabel = T
  { modPath
  , dirPath = DirPath.empty
  , knLabel
  }

equal :: T -> T -> Bool
equal kn1 kn2 = and
  [ Label.equal   (knLabel kn1) (knLabel kn2)
  , DirPath.equal (dirPath kn1) (dirPath kn2)
  , ModPath.equal (modPath kn1) (modPath kn2)
  ]
