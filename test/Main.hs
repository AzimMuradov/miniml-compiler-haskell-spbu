module Main where

import qualified Parser.ParserTest as ParserTest
import qualified TypeInference.TypeInferenceTest as TypeInferenceTest
import qualified System.Exit as Exit
import Test.HUnit (Counts (failures), runTestTT, Test (TestList))

main :: IO ()
main = do
  result <- runTestTT $ TestList [ParserTest.tests, TypeInferenceTest.tests]
  if failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess
