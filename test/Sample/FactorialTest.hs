{-# LANGUAGE OverloadedStrings #-}

module Sample.FactorialTest (tests) where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.IO as LBS
import Data.Text.Lazy (unpack)
import Parser.Ast
import Parser.Parser (parseProgram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.Pretty.Simple (pShowNoColor)
import Transformations.AnfPrettyPrinter (prettyPrint)
import Transformations.AstToAnf (astToAnf)
import TypeInference.PrettyPrint (pretty)
import TypeInference.Runtime (inferPolytype)

tests :: TestTree
tests =
  testGroup
    "factorial"
    [ testParsing "recursive factorial" facRec,
      testParsing "factorial with recursive loop" facRecLoop,
      testTypeInference "recursive factorial" facRec,
      testTypeInference "factorial with recursive loop" facRecLoop,
      testAstToAnf "recursive factorial" facRec,
      testAstToAnf "factorial with recursive loop" facRecLoop
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

testAstToAnf :: String -> (String -> FilePath) -> TestTree
testAstToAnf title testFileProvider =
  goldenVsString
    (title <> " - ANF")
    (testFileProvider "anf")
    (transformToBS <$> LBS.readFile (testFileProvider "ml"))

parseToBS :: Text -> ByteString
parseToBS = pack . unpack . pShowNoColor . parseProgram

evalToBS :: Text -> ByteString
evalToBS = pack . eval . parseProgram

transformToBS :: Text -> ByteString
transformToBS file = pack $ (prettyPrint . astToAnf) (fromJust (parseProgram file))

testFile :: String -> String
testFile filename = "test/Sample/Factorial/" <> filename

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
