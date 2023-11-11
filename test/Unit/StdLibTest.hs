{-# LANGUAGE OverloadedStrings #-}

module Unit.StdLibTest (tests) where

import Parser.Ast
import Parser.Parser (parseProgram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import TypeInference.PrettyPrint (pretty)
import TypeInference.TypeInference

tests :: TestTree
tests =
  testGroup
    "stdlib"
    [ testNotFunctionTypeInference
    ]

-- Tests

testNotFunctionTypeInference :: TestTree
testNotFunctionTypeInference =
  testCase "not function type inference" $
    do
      let expected = "bool -> bool"
      let actual = eval . parseProgram $ "not"

      expected @=? actual

-- Utils

eval :: Maybe Program -> String
eval s = case s of
  Nothing -> "Please, try again. Can't parse your program."
  Just p -> case inferProgram p of
    Left tyerr -> pretty tyerr
    Right ty -> pretty ty
