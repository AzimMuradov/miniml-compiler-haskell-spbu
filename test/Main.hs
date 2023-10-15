module Main where

import qualified Parser.ParserTest as ParserTest
import qualified System.Exit as Exit
import Test.HUnit (Counts (failures), Test (TestList), runTestTT)
import qualified TypeInference.TypeInferenceTest as TypeInferenceTest

main :: IO ()
main = do
  result <- runTestTT $ TestList [ParserTest.tests, TypeInferenceTest.tests]
  if failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess
