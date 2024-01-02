{-# LANGUAGE OverloadedStrings #-}

module Sample.FactorialTest (tests) where

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
    "factorial"
    ( (testParsing <$> factorials)
        <> (testTypeCheck <$> factorials)
        <> (testAstToAnf <$> factorials)
        <> [testLlvm facRec] -- TODO : (testLlvm <$> factorials)
        <> [testLlvmRun facRec] -- TODO : (testLlvmRun <$> factorials)
    )

-- Test types

testParsing :: TestFileProvider -> TestTree
testParsing (title, testFileProvider) =
  goldenVsString
    (title <> " - parsing")
    (testFileProvider "ast")
    (pack . unpack . pShowNoColor . processTillParser <$> LBS.readFile (testFileProvider "ml"))

testTypeCheck :: TestFileProvider -> TestTree
testTypeCheck (title, testFileProvider) =
  testCase (title <> " - type checking") $ do
    isOk <- processTillVerify <$> LBS.readFile (testFileProvider "ml")
    isOk @?= True

testAstToAnf :: TestFileProvider -> TestTree
testAstToAnf (title, testFileProvider) =
  goldenVsString
    (title <> " - ANF")
    (testFileProvider "anf")
    (pack . processTillAnfGen <$> LBS.readFile (testFileProvider "ml"))

testLlvm :: TestFileProvider -> TestTree
testLlvm (title, testFileProvider) =
  goldenVsString
    (title <> " - LLVM")
    (testFileProvider "ll")
    (pack . processTillLlvmIr "factorial" <$> LBS.readFile (testFileProvider "ml"))

testLlvmRun :: TestFileProvider -> TestTree
testLlvmRun (title, testFileProvider) =
  goldenVsString
    (title <> " - LLVM run")
    (testFileProvider "out")
    (pack . processTillLlvmRunOutput "factorial" <$> LBS.readFile (testFileProvider "ml"))

-- Test file providers

type TestFileProvider = (String, String -> FilePath)

factorials :: [TestFileProvider]
factorials = [facRec, facRecLoop, facRecCps]

facRec :: TestFileProvider
facRec = ("recursive factorial", \ext -> testFile $ "FacRec." <> ext)

facRecLoop :: TestFileProvider
facRecLoop = ("factorial with recursive loop", \ext -> testFile $ "FacRecLoop." <> ext)

facRecCps :: TestFileProvider
facRecCps = ("factorial with recursive cps", \ext -> testFile $ "FacRecCps." <> ext)

testFile :: String -> String
testFile filename = "test/Sample/Factorial/" <> filename
