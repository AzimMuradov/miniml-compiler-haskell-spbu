module Main where

import qualified Parser.ParserTest as ParserTest
import qualified System.Exit as Exit
import Test.HUnit (Counts (failures), runTestTT)

main :: IO ()
main = do
  result <- runTestTT ParserTest.tests
  if failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess
