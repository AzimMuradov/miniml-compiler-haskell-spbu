{-# LANGUAGE OverloadedStrings #-}

module Sample.FibonacciTest (tests) where

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
    "fibonacci"
    [ testGroup "parsing" (testParsing <$> fibs),
      testGroup "type checking" (testTypeCheck <$> fibs),
      testGroup "ANF" (testAstToAnf <$> fibs),
      testGroup "LLVM" (testLlvm <$> fibs),
      testGroup "LLVM run" (testLlvmRun <$> fibs)
    ]

-- Test types

testParsing :: TestFileProvider -> TestTree
testParsing (title, testFileProvider) =
  goldenVsString
    title
    (testFileProvider "ast")
    (pack . unpack . pShowNoColor . processTillParser <$> LBS.readFile (testFileProvider "ml"))

testTypeCheck :: TestFileProvider -> TestTree
testTypeCheck (title, testFileProvider) =
  testCase title $ do
    isOk <- processTillVerify <$> LBS.readFile (testFileProvider "ml")
    isOk @?= True

testAstToAnf :: TestFileProvider -> TestTree
testAstToAnf (title, testFileProvider) =
  goldenVsString
    title
    (testFileProvider "anf")
    (pack . processTillAnfGen <$> LBS.readFile (testFileProvider "ml"))

testLlvm :: TestFileProvider -> TestTree
testLlvm (title, testFileProvider) =
  goldenVsString
    title
    (testFileProvider "ll")
    (pack . processTillLlvmIr "fibonacci" <$> LBS.readFile (testFileProvider "ml"))

testLlvmRun :: TestFileProvider -> TestTree
testLlvmRun (title, testFileProvider) =
  goldenVsString
    title
    (testFileProvider "out")
    (pack . processTillLlvmRunOutput "fibonacci" <$> LBS.readFile (testFileProvider "ml"))

-- Test file providers

type TestFileProvider = (String, String -> FilePath)

fibs :: [TestFileProvider]
fibs = [fibRec, fibRecLoop, fibRecCps]

fibRec :: TestFileProvider
fibRec = ("recursive fibonacci", \ext -> testFile $ "FibRec." <> ext)

fibRecLoop :: TestFileProvider
fibRecLoop = ("fibonacci with recursive loop", \ext -> testFile $ "FibRecLoop." <> ext)

fibRecCps :: TestFileProvider
fibRecCps = ("fibonacci with recursive cps", \ext -> testFile $ "FibRecCps." <> ext)

testFile :: String -> String
testFile filename = "test/Sample/Fibonacci/" <> filename
