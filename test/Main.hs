{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Integration.FactorialTest
import Test.HUnit (Test (TestList))
import Test.Hspec (Spec)
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)
import qualified Unit.Parser.ParserTest as Unit.ParserTest
import qualified Unit.StdLibTest
import qualified Unit.TypeInference.TypeInferenceTest as Unit.TypeInferenceTest

main :: IO ()
main = do
  hspecTests <- testSpec "unit tests (hspec)" hspecSpec
  defaultMain $ testGroup "all tests" [hspecTests, goldenTests]

goldenTests :: TestTree
goldenTests =
  testGroup
    "integration tests (golden)"
    [ Integration.FactorialTest.tests
    ]

hspecSpec :: Spec
hspecSpec = fromHUnitTest hUnitTest

hUnitTest :: Test
hUnitTest =
  TestList
    [ Unit.ParserTest.tests,
      Unit.StdLibTest.tests,
      Unit.TypeInferenceTest.tests
    ]
