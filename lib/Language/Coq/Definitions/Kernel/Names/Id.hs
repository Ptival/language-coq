{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | coq/kernel/names.ml module Id

module Language.Coq.Definitions.Kernel.Names.Id (
  T,
  equal,
  ofString,
  ) where

import Data.Text.Prettyprint.Doc (hsep, parens, pretty)
import GHC.Generics              (Generic)

import PrettyPrinter.ToSExp      (ToSExp, toSExp)

newtype T = Id String
  deriving (Eq, Generic)

instance ToSExp T where
  toSExp (Id s) = parens $ hsep [pretty "Id", toSExp s]

equal :: T -> T -> Bool
equal = (==)

ofString :: String -> T
ofString = Id
