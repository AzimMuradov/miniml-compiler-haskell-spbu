module Main where

import qualified Integration.FactorialTest
import Test.HUnit (Test (TestList), (~:))
import Test.Hspec (describe, hspec)
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import qualified Unit.Parser.ParserTest as Unit.ParserTest
import qualified Unit.StdLibTest
import qualified Unit.TypeInference.TypeInferenceTest as Unit.TypeInferenceTest

main :: IO ()
main = hspec $ do
  describe "legacy HUnit tests" $ do
    fromHUnitTest hUnitTest

hUnitTest :: Test
hUnitTest = TestList [unitTest, integrationTest]
  where
    unitTest =
      "unit tests"
        ~: TestList
          [ Unit.ParserTest.tests,
            Unit.StdLibTest.tests,
            Unit.TypeInferenceTest.tests
          ]
    integrationTest =
      "integration tests"
        ~: TestList
          [ Integration.FactorialTest.tests
          ]
