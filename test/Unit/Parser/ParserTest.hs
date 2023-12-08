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
import Trees.Common

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
  let varDecl' x v = StmtDecl (DeclVar (x, Nothing) (ExprVal (ValInt v)))

  let varDecl x = varDecl' x 4
  let funDecl x args = StmtDecl (DeclFun x False (Fun ((,Nothing) <$> NonEmpty.fromList args) Nothing (ExprVal (ValInt 4))))
  let recFunDecl x args = StmtDecl (DeclFun x True (Fun ((,Nothing) <$> NonEmpty.fromList args) Nothing (ExprVal (ValInt 4))))

  let aDecl = varDecl "a"
  let bDecl = varDecl' "b" 8

  "let a = 4" ==?=> Just (Program [aDecl])
  "let a" ==?=> Nothing
  "let = 4" ==?=> Nothing
  "leta = 4" ==?=> Just (Program [StmtExpr (ExprBinOp (CompOp EqOp) (ExprId "leta") (ExprVal (ValInt 4)))])

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
              [ StmtDecl
                  ( DeclFun
                      "f"
                      False
                      ( Fun
                          (("a", Nothing) :| [])
                          Nothing
                          ( ExprBinOp
                              (ArithOp MulOp)
                              (ExprId "a")
                              ( ExprApp
                                  (ExprApp (ExprId "a") (ExprId "f"))
                                  (ExprVal (ValInt 4))
                              )
                          )
                      )
                  )
              ]
          )
  let declAndApp =
        Just
          ( Program
              [ StmtDecl
                  ( DeclFun
                      "f"
                      False
                      ( Fun
                          (("a", Nothing) :| [])
                          Nothing
                          (ExprBinOp (ArithOp MulOp) (ExprId "a") (ExprId "a"))
                      )
                  ),
                StmtExpr (ExprApp (ExprId "f") (ExprVal (ValInt 4)))
              ]
          )

  "let f a = a * a f 4" ==?=> decl
  "let f a = a * a\nf 4" ==?=> decl
  "let f a = a * a;;f 4" ==?=> declAndApp

testUnaryMinusOp :: TestTree
testUnaryMinusOp = testCase "unary minus operator" $ do
  let prgStmtExpr e = Just (Program [StmtExpr e])

  let zero = ExprVal (ValInt 0)
  let seven = ExprVal (ValInt 7)
  let a = ExprId "a"
  let b = ExprId "b"

  let minus = ExprUnOp UnMinusOp

  "-7" ==?=> prgStmtExpr (minus seven)
  "- 7" ==?=> prgStmtExpr (minus seven)
  "0 - 7" ==?=> prgStmtExpr (ExprBinOp (ArithOp MinusOp) zero seven)
  "0 - -7" ==?=> prgStmtExpr (ExprBinOp (ArithOp MinusOp) zero (minus seven))
  "0 - - 7" ==?=> prgStmtExpr (ExprBinOp (ArithOp MinusOp) zero (minus seven))
  "a - 7" ==?=> prgStmtExpr (ExprBinOp (ArithOp MinusOp) a seven)
  "a - b" ==?=> prgStmtExpr (ExprBinOp (ArithOp MinusOp) a b)
  "a - -b" ==?=> prgStmtExpr (ExprBinOp (ArithOp MinusOp) a (minus b))
  "a (-b)" ==?=> prgStmtExpr (ExprApp a (minus b))

(==?=>) :: Text -> Maybe Program -> Assertion
(==?=>) text maybeAst = assertEqual ("[" <> unpack text <> "]") maybeAst (parseProgram text)
