module Main where

import qualified FactorialTest
import qualified Parser.ParserTest as ParserTest
import qualified System.Exit as Exit
import Test.HUnit (Counts (failures), Test (TestList), runTestTT)

main :: IO ()
main = do
  result <-
    runTestTT $
      TestList
        [ FactorialTest.tests,
          ParserTest.tests
        ]
  if failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess
