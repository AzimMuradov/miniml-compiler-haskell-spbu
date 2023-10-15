module Parser.ParserTest (tests) where

import Data.Text (pack)
import Parser.Ast
import Parser.Parser (fileP, parse)
import Test.HUnit (Test (TestList), (~:), (~=?))

tests :: Test
tests =
  "parser"
    ~: TestList
      [ test1,
        test2,
        test3,
        test4
      ]

-- Tests

test1 :: Test
test1 =
  "[let a = 7]"
    ~: do
      let expected = Just [Program [SVarDecl (VarDecl (pack "a", Nothing) [EValue (VInt 7)])]]
      let actual = parseFile "let a = 7"

      expected ~=? actual

test2 :: Test
test2 =
  "[let = 7]"
    ~: do
      let expected = Nothing
      let actual = parseFile "let = 7"

      expected ~=? actual

test3 :: Test
test3 =
  "[let a]"
    ~: do
      let expected = Nothing
      let actual = parseFile "let a"

      expected ~=? actual

test4 :: Test
test4 =
  "[leta = 7]"
    ~: do
      let expected = Just [Program [SExpr (EOperations (ComparisonOp (EqOp {cL = EIdentifier (pack "leta"), cR = EValue (VInt 7)})))]]
      let actual = parseFile "leta = 7"

      expected ~=? actual

-- Utils

parseFile :: String -> Maybe [Program]
parseFile = parse fileP . pack
