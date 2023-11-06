{-# LANGUAGE OverloadedStrings #-}

module Sample.AnfTest (tests) where

import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Text (Text)
import qualified Data.Text.IO as LBS
import Data.Text.Lazy (unpack)
import Parser.Parser (parseProgram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Text.Pretty.Simple (pShowNoColor)
import Transformations.AstToAnf (astToAnf)

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
    (transformToBS <$> LBS.readFile (testFileProvider "ml"))

transformToBS :: Text -> ByteString
transformToBS file = pack $ unpack $ pShowNoColor (astToAnf <$> parseProgram file)

testFile :: String -> String
testFile filename = "test/Sample/Anf/" <> filename

simpleTest :: String -> String
simpleTest ext = testFile $ "SimpleTest." <> ext

hardTest :: String -> String
hardTest ext = testFile $ "HardTest." <> ext

testDuplicate :: String -> String
testDuplicate ext = testFile $ "DuplicateDeclaration." <> ext