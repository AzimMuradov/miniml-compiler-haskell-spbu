{-# LANGUAGE OverloadedStrings #-}

module Integration.FactorialTest (tests) where

import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text.IO as LBS
import Data.Text.Lazy (unpack)
import Parser.Ast
import Parser.Parser (parseProgram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.Pretty.Simple (pShowNoColor)
import TypeInference.PrettyPrint (pretty)
import TypeInference.Runtime (inferPolytype)

tests :: TestTree
tests =
  testGroup
    "factorial golden tests"
    [ goldenVsString
        "recursive factorial parsing"
        "test/Integration/Factorial/FacRec.ast"
        (parseToBS <$> LBS.readFile "test/Integration/Factorial/FacRec.ml"),
      goldenVsString
        "recursive factorial type inference"
        "test/Integration/Factorial/FacRec.ti"
        (evalToBS <$> LBS.readFile "test/Integration/Factorial/FacRec.ml"),
      goldenVsString
        "recursive factorial with the `loop` nested function parsing"
        "test/Integration/Factorial/FacRecLoop.ast"
        (parseToBS <$> LBS.readFile "test/Integration/Factorial/FacRecLoop.ml"),
      goldenVsString
        "recursive factorial with the `loop` nested function type inference"
        "test/Integration/Factorial/FacRecLoop.ti"
        (evalToBS <$> LBS.readFile "test/Integration/Factorial/FacRecLoop.ml")
    ]
  where
    parseToBS = pack . unpack . pShowNoColor . parseProgram
    evalToBS = pack . eval . parseProgram

eval :: Maybe Program -> String
eval s = case s of
  Nothing -> "Please, try again. Can't parse your program."
  Just p -> case inferPolytype p of
    Left tyerr -> pretty tyerr
    Right ty -> pretty ty
