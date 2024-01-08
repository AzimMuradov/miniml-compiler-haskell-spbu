module Sample.FactorialTest (tests) where

import Sample.Utils (TestFileProvider, testPhases)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "factorial"
    [ testPhases "recursive factorial" facRec,
      testPhases "factorial with recursive loop" facRecLoop,
      testPhases "factorial with recursive cps" facRecCps
    ]

facRec :: TestFileProvider
facRec ext = testFile $ "FacRec." <> ext

facRecLoop :: TestFileProvider
facRecLoop ext = testFile $ "FacRecLoop." <> ext

facRecCps :: TestFileProvider
facRecCps ext = testFile $ "FacRecCps." <> ext

testFile :: TestFileProvider
testFile filename = "test/Sample/Factorial/" <> filename
