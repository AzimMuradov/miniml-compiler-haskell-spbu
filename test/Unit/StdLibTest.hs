{-# LANGUAGE OverloadedStrings #-}

module Unit.StdLibTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Utils (processTillTypeChecker)

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
      let actual = processTillTypeChecker "not"

      expected @=? actual
