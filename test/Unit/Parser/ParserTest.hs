{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Unit.Parser.ParserTest (tests) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text, unpack)
import Parser.Ast
import Parser.Parser (parseProgram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "parser"
    [ testLetDecls,
      testWhitespace,
      testUnaryMinusOp
    ]

-- Tests

testLetDecls :: TestTree
testLetDecls = testCase "let declarations" $ do
  let varDecl' x v = StmtUserDecl (DeclVar (x, Nothing) (ExprValue (ValInt v)))

  let varDecl x = varDecl' x 4
  let funDecl x args = StmtUserDecl (DeclFun x (Fun ((,Nothing) <$> NonEmpty.fromList args) Nothing (ExprValue (ValInt 4))))
  let recFunDecl x args = StmtUserDecl (DeclRecFun x (Fun ((,Nothing) <$> NonEmpty.fromList args) Nothing (ExprValue (ValInt 4))))

  let aDecl = varDecl "a"
  let bDecl = varDecl' "b" 8

  "let a = 4" ==?=> Just (Program [aDecl])
  "let a" ==?=> Nothing
  "let = 4" ==?=> Nothing
  "leta = 4" ==?=> Just (Program [StmtExpr (ExprBinaryOperation (ComparisonOp EqOp) (ExprIdentifier "leta") (ExprValue (ValInt 4)))])

  "let rec a = 4" ==?=> Nothing
  "let rec a b = 4" ==?=> Just (Program [recFunDecl "a" ["b"]])
  "let reca a b = 4" ==?=> Just (Program [funDecl "reca" ["a", "b"]])
  "leta rec a b = 4" ==?=> Nothing
  "letaa rec a b = 4" ==?=> Nothing
  "let reca = 4" ==?=> Just (Program [varDecl "reca"])
  "let recaa = 4" ==?=> Just (Program [varDecl "recaa"])
  "let let a = 4" ==?=> Nothing
  "let let = 4" ==?=> Nothing
  "let leta = 4" ==?=> Just (Program [varDecl "leta"])

  "let a = 4 let b = 8" ==?=> Just (Program [aDecl, bDecl])
  "let a = 4\nlet b = 8" ==?=> Just (Program [aDecl, bDecl])
  "let a = 4;;let b = 8" ==?=> Just (Program [aDecl, bDecl])
  ";;let a = 4;;;;let b = 8;;;;;; ;;" ==?=> Just (Program [aDecl, bDecl])

testWhitespace :: TestTree
testWhitespace = testCase "whitespace" $ do
  let decl =
        Just
          ( Program
              [ StmtUserDecl
                  ( DeclFun
                      "f"
                      ( Fun
                          (("a", Nothing) :| [])
                          Nothing
                          ( ExprBinaryOperation
                              (ArithmeticOp MulOp)
                              (ExprIdentifier "a")
                              ( ExprApplication
                                  (ExprApplication (ExprIdentifier "a") (ExprIdentifier "f"))
                                  (ExprValue (ValInt 4))
                              )
                          )
                      )
                  )
              ]
          )
  let declAndApp =
        Just
          ( Program
              [ StmtUserDecl
                  ( DeclFun
                      "f"
                      ( Fun
                          (("a", Nothing) :| [])
                          Nothing
                          (ExprBinaryOperation (ArithmeticOp MulOp) (ExprIdentifier "a") (ExprIdentifier "a"))
                      )
                  ),
                StmtExpr (ExprApplication (ExprIdentifier "f") (ExprValue (ValInt 4)))
              ]
          )

  "let f a = a * a f 4" ==?=> decl
  "let f a = a * a\nf 4" ==?=> decl
  "let f a = a * a;;f 4" ==?=> declAndApp

testUnaryMinusOp :: TestTree
testUnaryMinusOp = testCase "unary minus operator" $ do
  let prgStmtExpr e = Just (Program [StmtExpr e])

  let zero = ExprValue (ValInt 0)
  let seven = ExprValue (ValInt 7)
  let a = ExprIdentifier "a"
  let b = ExprIdentifier "b"

  let minus = ExprUnaryOperation UnaryMinusOp

  "-7" ==?=> prgStmtExpr (minus seven)
  "- 7" ==?=> prgStmtExpr (minus seven)
  "0 - 7" ==?=> prgStmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) zero seven)
  "0 - -7" ==?=> prgStmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) zero (minus seven))
  "0 - - 7" ==?=> prgStmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) zero (minus seven))
  "a - 7" ==?=> prgStmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) a seven)
  "a - b" ==?=> prgStmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) a b)
  "a - -b" ==?=> prgStmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) a (minus b))
  "a (-b)" ==?=> prgStmtExpr (ExprApplication a (minus b))

(==?=>) :: Text -> Maybe Program -> Assertion
(==?=>) text maybeAst = assertEqual ("[" <> unpack text <> "]") maybeAst (parseProgram text)
