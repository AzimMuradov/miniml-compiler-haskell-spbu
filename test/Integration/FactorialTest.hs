{-# LANGUAGE OverloadedStrings #-}

module Integration.FactorialTest (tests) where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Text (Text)
import qualified Data.Text.IO as LBS
import Data.Text.Lazy (unpack)
import Parser.Ast
import Parser.Parser (parseProgram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.Pretty.Simple (pShowNoColor)
import TypeInference.PrettyPrint (pretty)
import TypeInference.Runtime (inferPolytype)

tests :: TestTree
tests =
  testGroup
    "factorial golden tests"
    [ testParsing "recursive factorial" facRec,
      testParsing "factorial with recursive loop" facRecLoop,
      testTypeInference "recursive factorial" facRec,
      testTypeInference "factorial with recursive loop" facRecLoop
    ]

testParsing :: String -> (String -> FilePath) -> TestTree
testParsing title testFileProvider =
  goldenVsString
    (title <> " - parsing")
    (testFileProvider "ast")
    (parseToBS <$> LBS.readFile (testFileProvider "ml"))

testTypeInference :: String -> (String -> FilePath) -> TestTree
testTypeInference title testFileProvider =
  goldenVsString
    (title <> " - type inference")
    (testFileProvider "ti")
    (evalToBS <$> LBS.readFile (testFileProvider "ml"))

parseToBS :: Text -> ByteString
parseToBS = pack . unpack . pShowNoColor . parseProgram

evalToBS :: Text -> ByteString
evalToBS = pack . eval . parseProgram

testFile :: String -> String
testFile filename = "test/Integration/Factorial/" <> filename

facRec :: String -> String
facRec ext = testFile $ "FacRec." <> ext

facRecLoop :: String -> String
facRecLoop ext = testFile $ "FacRecLoop." <> ext

eval :: Maybe Program -> String
eval s = case s of
  Nothing -> "Please, try again. Can't parse your program."
  Just p -> case inferPolytype p of
    Left tyerr -> pretty tyerr
    Right ty -> pretty ty
