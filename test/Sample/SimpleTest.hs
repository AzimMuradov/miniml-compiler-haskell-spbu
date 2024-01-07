{-# LANGUAGE OverloadedStrings #-}

module Sample.SimpleTest (tests) where

import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Text.IO as LBS
import Data.Text.Lazy (unpack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Pretty.Simple (pShowNoColor)
import Utils (processTillAnfGen, processTillLlvmIr, processTillLlvmRunOutput, processTillParser, processTillVerify)

tests :: TestTree
tests =
  testGroup
    "simple test"
    [ testParsing simpleTest,
      testTypeCheck simpleTest,
      testAstToAnf simpleTest,
      testLlvm simpleTest,
      testLlvmRun simpleTest
    ]

-- Test types

testParsing :: TestFileProvider -> TestTree
testParsing testFileProvider =
  goldenVsString
    "parsing"
    (testFileProvider "ast")
    (pack . unpack . pShowNoColor . processTillParser <$> LBS.readFile (testFileProvider "ml"))

testTypeCheck :: TestFileProvider -> TestTree
testTypeCheck testFileProvider =
  testCase "type checking" $ do
    isOk <- processTillVerify <$> LBS.readFile (testFileProvider "ml")
    isOk @?= True

testAstToAnf :: TestFileProvider -> TestTree
testAstToAnf testFileProvider =
  goldenVsString
    "ANF"
    (testFileProvider "anf")
    (pack . processTillAnfGen <$> LBS.readFile (testFileProvider "ml"))

testLlvm :: TestFileProvider -> TestTree
testLlvm testFileProvider =
  goldenVsString
    "LLVM"
    (testFileProvider "ll")
    (pack . processTillLlvmIr "simpleTest" <$> LBS.readFile (testFileProvider "ml"))

testLlvmRun :: TestFileProvider -> TestTree
testLlvmRun testFileProvider =
  goldenVsString
    "LLVM run"
    (testFileProvider "out")
    (pack . processTillLlvmRunOutput "simpleTest" <$> LBS.readFile (testFileProvider "ml"))

-- Test file providers

type TestFileProvider = String -> FilePath

simpleTest :: TestFileProvider
simpleTest ext = testFile $ "SimpleTest." <> ext

testFile :: String -> String
testFile filename = "test/Sample/Simple/" <> filename
