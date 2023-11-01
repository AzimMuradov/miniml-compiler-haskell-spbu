{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Integration.FactorialTest
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Unit.Parser.ParserTest as Unit.ParserTest
import qualified Unit.StdLibTest
import qualified Unit.TypeInference.TypeInferenceTest as Unit.TypeInferenceTest

main :: IO ()
main = defaultMain $ testGroup "all tests" [unitTests, goldenTests]

unitTests :: TestTree
unitTests =
  testGroup
    "unit tests"
    [ Unit.ParserTest.tests,
      Unit.StdLibTest.tests,
      Unit.TypeInferenceTest.tests
    ]

goldenTests :: TestTree
goldenTests =
  testGroup
    "integration tests (golden)"
    [ Integration.FactorialTest.tests
    ]
