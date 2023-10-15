{-# LANGUAGE OverloadedStrings #-}

module Parser.ParserTest (tests) where

import Data.Text (Text)
import Parser.Ast
import Parser.Parser (parse, programP)
import Test.HUnit (Test (TestList), (~:), (~=?))

tests :: Test
tests =
  "parser"
    ~: TestList
      [ test0,
        test1,
        test2,
        test3,
        test4,
        test5,
        test6,
        test7,
        test8
      ]

-- Tests

test0 :: Test
test0 =
  "[let a = 7]"
    ~: do
      let expected = Just $ Program [SVarDecl (VarDecl ("a", Nothing) [EValue (VInt 7)])]
      let actual = parseProgram "let a = 7"

      expected ~=? actual

test1 :: Test
test1 =
  "[let = 7]"
    ~: do
      let expected = Nothing
      let actual = parseProgram "let = 7"

      expected ~=? actual

test2 :: Test
test2 =
  "[let a]"
    ~: do
      let expected = Nothing
      let actual = parseProgram "let a"

      expected ~=? actual

test3 :: Test
test3 =
  "[leta = 7]"
    ~: do
      let expected = Just $ Program [SExpr (EOperations (ComparisonOp (EqOp {cL = EIdentifier "leta", cR = EValue (VInt 7)})))]
      let actual = parseProgram "leta = 7"

      expected ~=? actual

test4 :: Test
test4 =
  "[let a = 4 let b = 8]"
    ~: do
      let expected =
            Just $
              Program
                [ SVarDecl (VarDecl ("a", Nothing) [EValue (VInt 4)]),
                  SVarDecl (VarDecl ("b", Nothing) [EValue (VInt 8)])
                ]
      let actual = parseProgram "let a = 4 let b = 8"

      expected ~=? actual

test5 :: Test
test5 =
  "[let a = 4\nlet b = 8]"
    ~: do
      let expected =
            Just $
              Program
                [ SVarDecl (VarDecl ("a", Nothing) [EValue (VInt 4)]),
                  SVarDecl (VarDecl ("b", Nothing) [EValue (VInt 8)])
                ]
      let actual = parseProgram "let a = 4\nlet b = 8"

      expected ~=? actual

test6 :: Test
test6 =
  "[let a = 4;;let b = 8]"
    ~: do
      let expected =
            Just $
              Program
                [ SVarDecl (VarDecl ("a", Nothing) [EValue (VInt 4)]),
                  SVarDecl (VarDecl ("b", Nothing) [EValue (VInt 8)])
                ]
      let actual = parseProgram "let a = 4;;let b = 8"

      expected ~=? actual

test7 :: Test
test7 =
  "[let f a = a * a f 4]"
    ~: do
      let expected =
            Just $
              Program
                [ SFunDecl
                    ( FunDecl
                        "f"
                        ( Fun
                            [("a", Nothing)]
                            [ EOperations
                                ( ArithmeticOp
                                    ( MulOp
                                        { aL = EIdentifier "a",
                                          aR =
                                            EApplication
                                              (EApplication (EIdentifier "a") (EIdentifier "f"))
                                              (EValue (VInt 4))
                                        }
                                    )
                                )
                            ]
                        )
                    )
                ]
      let actual = parseProgram "let f a = a * a f 4"

      expected ~=? actual

test8 :: Test
test8 =
  "[let f a = a * a;;f 4]"
    ~: do
      let expected =
            Just $
              Program
                [ SFunDecl
                    ( FunDecl
                        "f"
                        ( Fun
                            [("a", Nothing)]
                            [EOperations (ArithmeticOp (MulOp {aL = EIdentifier "a", aR = EIdentifier "a"}))]
                        )
                    ),
                  SExpr (EApplication (EIdentifier "f") (EValue (VInt 4)))
                ]
      let actual = parseProgram "let f a = a * a;;f 4"

      expected ~=? actual

-- Utils

parseProgram :: Text -> Maybe Program
parseProgram = parse programP
