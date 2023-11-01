{-# LANGUAGE OverloadedStrings #-}

module Unit.Parser.ParserTest (tests) where

import qualified Data.List.NonEmpty as NonEmpty
import Parser.Ast
import Parser.Parser (parseProgram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

tests :: TestTree
tests =
  testGroup
    "parser"
    [ test0,
      test1,
      test2,
      test3,
      test4,
      test5,
      test6,
      test7,
      test8,
      testUnaryMinusOp
    ]

-- Tests

test0 :: TestTree
test0 =
  testCase "[let a = 7]" $ do
    let expected = Just $ Program [StmtUserDecl (DeclVar ("a", Nothing) (ExprValue (ValInt 7)))]
    let actual = parseProgram "let a = 7"

    expected @=? actual

test1 :: TestTree
test1 =
  testCase "[let = 7]" $
    do
      let expected = Nothing
      let actual = parseProgram "let = 7"

      expected @=? actual

test2 :: TestTree
test2 = testCase
  "[let a]"
  $ do
    let expected = Nothing
    let actual = parseProgram "let a"

    expected @=? actual

test3 :: TestTree
test3 = testCase
  "[leta = 7]"
  $ do
    let expected = Just $ Program [StmtExpr (ExprBinaryOperation (ComparisonOp EqOp) (ExprIdentifier "leta") (ExprValue (ValInt 7)))]
    let actual = parseProgram "leta = 7"

    expected @=? actual

test4 :: TestTree
test4 = testCase
  "[let a = 4 let b = 8]"
  $ do
    let expected =
          Just $
            Program
              [ StmtUserDecl (DeclVar ("a", Nothing) (ExprValue (ValInt 4))),
                StmtUserDecl (DeclVar ("b", Nothing) (ExprValue (ValInt 8)))
              ]
    let actual = parseProgram "let a = 4 let b = 8"

    expected @=? actual

test5 :: TestTree
test5 = testCase
  "[let a = 4\\nlet b = 8]"
  $ do
    let expected =
          Just $
            Program
              [ StmtUserDecl (DeclVar ("a", Nothing) (ExprValue (ValInt 4))),
                StmtUserDecl (DeclVar ("b", Nothing) (ExprValue (ValInt 8)))
              ]
    let actual = parseProgram "let a = 4\nlet b = 8"

    expected @=? actual

test6 :: TestTree
test6 = testCase
  "[let a = 4;;let b = 8]"
  $ do
    let expected =
          Just $
            Program
              [ StmtUserDecl (DeclVar ("a", Nothing) (ExprValue (ValInt 4))),
                StmtUserDecl (DeclVar ("b", Nothing) (ExprValue (ValInt 8)))
              ]
    let actual = parseProgram "let a = 4;;let b = 8"

    expected @=? actual

test7 :: TestTree
test7 = testCase
  "[let f a = a * a f 4]"
  $ do
    let expected =
          Just $
            Program
              [ StmtUserDecl
                  ( DeclFun
                      "f"
                      ( Fun
                          (NonEmpty.singleton ("a", Nothing))
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
    let actual = parseProgram "let f a = a * a f 4"

    expected @=? actual

test8 :: TestTree
test8 = testCase
  "[let f a = a * a;;f 4]"
  $ do
    let expected =
          Just $
            Program
              [ StmtUserDecl
                  ( DeclFun
                      "f"
                      ( Fun
                          (NonEmpty.singleton ("a", Nothing))
                          Nothing
                          (ExprBinaryOperation (ArithmeticOp MulOp) (ExprIdentifier "a") (ExprIdentifier "a"))
                      )
                  ),
                StmtExpr (ExprApplication (ExprIdentifier "f") (ExprValue (ValInt 4)))
              ]
    let actual = parseProgram "let f a = a * a;;f 4"

    expected @=? actual

testUnaryMinusOp :: TestTree
testUnaryMinusOp = testCase
  "[unary minus operator]"
  $ do
    let expected =
          Just $
            Program
              [ StmtExpr (ExprUnaryOperation UnaryMinusOp (ExprValue (ValInt 7))),
                StmtExpr (ExprUnaryOperation UnaryMinusOp (ExprValue (ValInt 7))),
                StmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) (ExprValue (ValInt 0)) (ExprValue (ValInt 7))),
                StmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) (ExprValue (ValInt 0)) (ExprUnaryOperation UnaryMinusOp (ExprValue (ValInt 7)))),
                StmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) (ExprValue (ValInt 0)) (ExprUnaryOperation UnaryMinusOp (ExprValue (ValInt 7)))),
                StmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) (ExprIdentifier "a") (ExprValue (ValInt 7))),
                StmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) (ExprIdentifier "a") (ExprIdentifier "b")),
                StmtExpr (ExprBinaryOperation (ArithmeticOp MinusOp) (ExprIdentifier "a") (ExprUnaryOperation UnaryMinusOp (ExprIdentifier "b"))),
                StmtExpr (ExprApplication (ExprIdentifier "a") (ExprUnaryOperation UnaryMinusOp (ExprIdentifier "b")))
              ]
    let actual = parseProgram "-7;;- 7;; 0 - 7;; 0 - -7;; 0 - - 7;; a - 7;; a - b;; a - -b;; a (-b);;"

    expected @=? actual
