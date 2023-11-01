module Main where

import qualified FactorialTest
import qualified Parser.ParserTest as ParserTest
import qualified StdLibTest
import Test.HUnit (Test (TestList))
import Test.Hspec (describe, hspec)
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import qualified TypeInference.TypeInferenceTest as TypeInferenceTest

main :: IO ()
main = hspec $ do
  describe "legacy HUnit tests" $ do
    fromHUnitTest hUnitTest

hUnitTest :: Test
hUnitTest =
  TestList
    [ FactorialTest.tests,
      ParserTest.tests,
      StdLibTest.tests,
      TypeInferenceTest.tests
    ]
