{-# LANGUAGE OverloadedStrings #-}

module StdLibTest (tests) where

import Data.Text (Text)
import Parser.Ast
import Parser.Parser (parseProgram)
import Test.HUnit (Test (TestList), (~:), (~=?))
import TypeInference.PrettyPrint (pretty)
import TypeInference.Runtime (inferPolytype)

tests :: Test
tests =
  "stdlib"
    ~: TestList
      [ testNotFunctionTypeInference
      ]

-- Tests

testNotFunctionTypeInference :: Test
testNotFunctionTypeInference =
  "not function type inference"
    ~: do
      let expected = "bool -> bool"
      let actual = eval . parseProgram $ "not"

      expected ~=? actual

-- Utils

eval :: Maybe Program -> String
eval s = case s of
  Nothing -> "Please, try again. Can't parse your program."
  Just p -> case inferPolytype p of
    Left tyerr -> pretty tyerr
    Right ty -> pretty ty
