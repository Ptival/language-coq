{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- | NOTE: this requires `dune exec sertop` to work!

module PrettyPrinter.ToCoqString (
  ) where

import           Control.Monad                              (forM)
import           Data.List                                  (isInfixOf)
import           Data.SCargot
import           Data.SCargot.Repr
import           Data.Text                                  (Text, pack, unpack)
import           System.IO                                  (hClose, hGetContents, hPutStr)
import           System.Process                             (runInteractiveProcess)
import Text.Parsec

import           PrettyPrinter.ToSExp                       (ToSExp, toSExp)
import qualified Language.Coq.Definitions.Vernac.VernacExpr as VernacExpr

-- TODO: check that this works with nested strings
readString :: Reader Text
readString _ = do
  s <- manyTill anyChar (try (string "\""))
  return $ SAtom $ pack s

mySpec :: SExprParser Text (WellFormedSExpr Text)
mySpec =
  asWellFormed
  $ addReader '"' readString
  $ mkParser (pack <$> many1 alphaNum)

decodeSExp :: String -> Either String (WellFormedSExpr Text)
decodeSExp s = decodeOne mySpec (pack s)

-- This is super inefficient! :-)
class ToCoqString a where
  toCoqString :: a -> IO String

  default
    toCoqString :: (ToSExp a) => a -> IO String
  toCoqString a = do
    let sexp = toSExp a
    (inp, out, _, _) <- runInteractiveProcess
                        "dune"
                        ["exec", "sertop"]
                        (Just "/Users/val/Personal/PeaCoq/coq-serapi")
                        Nothing
    let query = "(Print ((pp_format PpStr)) (CoqAst (() " ++ show sexp ++ ")))"
    hPutStr inp query
    hClose inp
    contents <- hGetContents out
    let contentsLines = lines contents
    putStrLn query
    forM contentsLines putStrLn
    case filter ("ObjList" `isInfixOf`) contentsLines of
      []  -> error "No line matched"
      [r] -> do
        case decodeSExp r of
          Right (
            WFSList [ WFSAtom "Answer"
                    , WFSAtom "0"
                    , WFSList [ WFSAtom "ObjList"
                              , WFSList [ WFSList [ WFSAtom "CoqString"
                                                 , WFSAtom s
                                                 ]
                                        ]
                              ]
                    ]
            ) -> return $ unpack s
          _ -> error $ "The output format did not match our expectation...\n" ++ show (decodeSExp r)
      _   -> error "Too many lines matched"

instance ToCoqString VernacExpr.VernacControl where

test :: IO ()
test = do
  print =<< toCoqString VernacExpr.testVernacControl
