{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | coq/library/libnames.ml

module Language.Coq.Definitions.Library.LibNames (
  QualId,
  ) where

import           GHC.Generics                                  (Generic)

import qualified Language.Coq.Definitions.Kernel.Names.DirPath as DirPath
import qualified Language.Coq.Definitions.Kernel.Names.Id      as Id
import qualified Language.Coq.Definitions.Lib.CAST             as CAST
import           PrettyPrinter.ToSExp                          (ToSExp)

data FullPath = FullPath
  { dirPath  :: DirPath.T
  , baseName :: Id.T
  }
  deriving (Generic, ToSExp)

type QualIdR = FullPath
type QualId  = CAST.T QualIdR
