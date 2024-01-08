module Sample.SimpleTest (tests) where

import Sample.Utils (TestFileProvider, testPhases)
import Test.Tasty (TestTree)

tests :: TestTree
tests = testPhases "simple test" simpleTest

simpleTest :: TestFileProvider
simpleTest ext = testFile $ "SimpleTest." <> ext

testFile :: TestFileProvider
testFile filename = "test/Sample/Simple/" <> filename
