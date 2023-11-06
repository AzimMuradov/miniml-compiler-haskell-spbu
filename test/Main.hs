{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Sample.AnfTest
import qualified Sample.FactorialTest
import Test.Tasty (TestTree, defaultMain, testGroup)
import qualified Unit.Parser.ParserTest as Unit.ParserTest
import qualified Unit.StdLibTest
import qualified Unit.TypeInference.TypeInferenceTest as Unit.TypeInferenceTest

main :: IO ()
main = defaultMain $ testGroup "all tests" [unitTests, sampleTests]

unitTests :: TestTree
unitTests =
  testGroup
    "unit tests (HUnit)"
    [ Unit.ParserTest.tests,
      Unit.StdLibTest.tests,
      Unit.TypeInferenceTest.tests
    ]

sampleTests :: TestTree
sampleTests =
  testGroup
    "sample tests (Golden)"
    [ Sample.FactorialTest.tests,
      Sample.AnfTest.tests
    ]
