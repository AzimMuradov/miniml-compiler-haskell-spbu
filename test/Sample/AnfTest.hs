{-# LANGUAGE OverloadedStrings #-}

module Sample.AnfTest (tests) where

import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text.IO as LBS
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Utils (processTillAnfGen)

tests :: TestTree
tests =
  testGroup
    "anf"
    [ testAstToAnf "simpleTest" simpleTest,
      testAstToAnf "hardTest" hardTest,
      testAstToAnf "duplicateTest" testDuplicate
    ]

testAstToAnf :: String -> (String -> FilePath) -> TestTree
testAstToAnf title testFileProvider =
  goldenVsString
    (title <> " - ANF")
    (testFileProvider "anf")
    (pack . processTillAnfGen <$> LBS.readFile (testFileProvider "ml"))

testFile :: String -> String
testFile filename = "test/Sample/Anf/" <> filename

simpleTest :: String -> String
simpleTest ext = testFile $ "SimpleTest." <> ext

hardTest :: String -> String
hardTest ext = testFile $ "HardTest." <> ext

testDuplicate :: String -> String
testDuplicate ext = testFile $ "DuplicateDeclaration." <> ext
