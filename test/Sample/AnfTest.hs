module Sample.AnfTest (tests) where

import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text.IO as LBS
import Sample.Utils (TestFileProvider)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Utils (processTillAnfGen)

tests :: TestTree
tests =
  testGroup
    "ANF"
    [ testAstToAnf "simple test" simpleTest,
      testAstToAnf "hard test" hardTest,
      testAstToAnf "duplicate test" testDuplicate
    ]

testAstToAnf :: String -> TestFileProvider -> TestTree
testAstToAnf title testFileProvider =
  goldenVsString
    title
    (testFileProvider "anf")
    (pack . processTillAnfGen <$> LBS.readFile (testFileProvider "ml"))

simpleTest :: TestFileProvider
simpleTest ext = testFile $ "SimpleTest." <> ext

hardTest :: TestFileProvider
hardTest ext = testFile $ "HardTest." <> ext

testDuplicate :: TestFileProvider
testDuplicate ext = testFile $ "DuplicateDeclaration." <> ext

testFile :: TestFileProvider
testFile filename = "test/Sample/Anf/" <> filename
