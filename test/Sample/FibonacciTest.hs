module Sample.FibonacciTest (tests) where

import Sample.Utils (TestFileProvider, testPhases)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "fibonacci"
    [ testPhases "recursive fibonacci" fibRec,
      testPhases "fibonacci with recursive loop" fibRecLoop,
      testPhases "fibonacci with recursive cps" fibRecCps
    ]

fibRec :: TestFileProvider
fibRec ext = testFile $ "FibRec." <> ext

fibRecLoop :: TestFileProvider
fibRecLoop ext = testFile $ "FibRecLoop." <> ext

fibRecCps :: TestFileProvider
fibRecCps ext = testFile $ "FibRecCps." <> ext

testFile :: TestFileProvider
testFile filename = "test/Sample/Fibonacci/" <> filename
