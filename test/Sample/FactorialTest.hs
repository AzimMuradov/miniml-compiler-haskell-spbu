{-# LANGUAGE OverloadedStrings #-}

module Sample.FactorialTest (tests) where

import CodeGen.Llvm.Ir2LlvmIr (genLlvmIrModule, ppLlvmModule)
import CodeGen.Module
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
import Transformations.Anf.AnfGen (astToAnf)
import Transformations.Anf.PrettyPrinter (prettyPrint)
import TypeInference.PrettyPrint (pretty)
import TypeInference.TypeInference (inferProgram)

tests :: TestTree
tests =
  testGroup
    "factorial"
    [ testParsing "recursive factorial" facRec,
      testParsing "factorial with recursive loop" facRecLoop,
      testParsing "factorial with recursive cps" facRecCps,
      testTypeInference "recursive factorial" facRec,
      testTypeInference "factorial with recursive loop" facRecLoop,
      testTypeInference "factorial with recursive cps" facRecCps,
      testAstToAnf "recursive factorial" facRec,
      testAstToAnf "factorial with recursive loop" facRecLoop,
      testAstToAnf "factorial with recursive cps" facRecCps,
      testLlvm "recursive factorial" facRec
      -- TODO : testLlvm "factorial with recursive loop" facRecLoop,
      -- TODO : testLlvm "factorial with recursive cps" facRecCps
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

testLlvm :: String -> (String -> FilePath) -> TestTree
testLlvm title testFileProvider =
  goldenVsString
    (title <> " - LLVM")
    (testFileProvider "ll")
    (llvmTestActual <$> LBS.readFile (testFileProvider "ml"))

parseToBS :: Text -> ByteString
parseToBS = pack . unpack . pShowNoColor . parseProgram

evalToBS :: Text -> ByteString
evalToBS = pack . eval . parseProgram

transformToBS :: Text -> ByteString
transformToBS file = pack $ (prettyPrint . astToAnf) (fromJust (parseProgram file))

llvmTestActual :: Text -> ByteString
llvmTestActual file =
  let ast = fromJust (parseProgram file)
      anf = astToAnf ast
      irMod = Module "factorial" anf
      llvm = ppLlvmModule $ genLlvmIrModule irMod
   in pack $ unpack llvm

testFile :: String -> String
testFile filename = "test/Sample/Factorial/" <> filename

facRec :: String -> String
facRec ext = testFile $ "FacRec." <> ext

facRecLoop :: String -> String
facRecLoop ext = testFile $ "FacRecLoop." <> ext

facRecCps :: String -> String
facRecCps ext = testFile $ "FacRecCps." <> ext

eval :: Maybe Program -> String
eval s = case s of
  Nothing -> "Please, try again. Can't parse your program."
  Just p -> case inferProgram p of
    Left tyerr -> pretty tyerr
    Right ty -> pretty ty
