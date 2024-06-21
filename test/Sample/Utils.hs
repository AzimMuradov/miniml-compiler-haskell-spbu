{-# LANGUAGE OverloadedStrings #-}

module Sample.Utils where

import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.Text.IO as TxtIO
import qualified Data.Text.Lazy as LazyTxt
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Pretty.Simple (pShowNoColor)
import Utils
  ( processTillAnfGen,
    processTillLlvmIr,
    processTillLlvmRunOutput,
    processTillParser,
    processTillVerify,
  )

type TestFileProvider = String -> FilePath

testPhases :: TestName -> TestFileProvider -> TestTree
testPhases name testFileProvider =
  testGroup
    name
    [ testParsing testFileProvider,
      testTypeCheck testFileProvider,
      testAstToAnf testFileProvider,
      testLlvm testFileProvider,
      testLlvmRun testFileProvider
    ]

testParsing :: TestFileProvider -> TestTree
testParsing testFileProvider =
  goldenVsString
    "parsing"
    (testFileProvider "ast")
    (LBSC8.pack . LazyTxt.unpack . pShowNoColor . processTillParser <$> TxtIO.readFile (testFileProvider "ml"))

testTypeCheck :: TestFileProvider -> TestTree
testTypeCheck testFileProvider =
  testCase "type checking" $ do
    isOk <- processTillVerify <$> TxtIO.readFile (testFileProvider "ml")
    isOk @?= True

testAstToAnf :: TestFileProvider -> TestTree
testAstToAnf testFileProvider =
  goldenVsString
    "ANF"
    (testFileProvider "anf")
    (LBSC8.pack . processTillAnfGen <$> TxtIO.readFile (testFileProvider "ml"))

testLlvm :: TestFileProvider -> TestTree
testLlvm testFileProvider =
  goldenVsString
    "LLVM"
    (testFileProvider "ll")
    (LBSC8.pack . processTillLlvmIr <$> TxtIO.readFile (testFileProvider "ml"))

testLlvmRun :: TestFileProvider -> TestTree
testLlvmRun testFileProvider =
  goldenVsString
    "LLVM run"
    (testFileProvider "out")
    (LBSC8.pack . processTillLlvmRunOutput <$> TxtIO.readFile (testFileProvider "ml"))
