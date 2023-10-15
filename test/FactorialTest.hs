{-# LANGUAGE OverloadedStrings #-}

module FactorialTest (tests) where

import Data.Text (Text)
import Parser.Ast
import Parser.Parser (parse, programP)
import Test.HUnit (Test (TestList), (~:), (~=?))

tests :: Test
tests = "factorial" ~: TestList [testFacRec, testFacRecLoop]

-- Tests

testFacRec :: Test
testFacRec =
  "recursive factorial"
    ~: do
      let expected =
            Just $
              Program
                [ StmtRecFunDecl $
                    RecFunDecl
                      "factorial"
                      ( Fun
                          [("n", Nothing)]
                          Nothing
                          ( ExprIf
                              (ExprOperations (ComparisonOp (LeOp {cL = ExprIdentifier "n", cR = ExprValue (ValInt 0)})))
                              (ExprValue (ValInt 1))
                              ( ExprOperations
                                  ( ArithmeticOp
                                      ( MulOp
                                          { aL = ExprIdentifier "n",
                                            aR = ExprApplication (ExprIdentifier "factorial") (ExprOperations (ArithmeticOp (MinusOp {aL = ExprIdentifier "n", aR = ExprValue (ValInt 1)})))
                                          }
                                      )
                                  )
                              )
                          )
                      )
                ]
      let actual = parseProgram "let rec factorial n = if n <= 0 then 1 else n * factorial (n - 1)"

      expected ~=? actual

testFacRecLoop :: Test
testFacRecLoop =
  "recursive factorial with the `loop` nested function"
    ~: do
      let expected =
            Just $
              Program
                [ StmtFunDecl $
                    FunDecl
                      "factorial"
                      ( Fun
                          [("n", Nothing)]
                          Nothing
                          ( ExprLetRecInF
                              "loop"
                              ( Fun
                                  [("i", Nothing), ("accum", Nothing)]
                                  Nothing
                                  ( ExprIf
                                      (ExprOperations (ComparisonOp (MtOp {cL = ExprIdentifier "i", cR = ExprIdentifier "n"})))
                                      (ExprIdentifier "accum")
                                      ( ExprApplication
                                          (ExprApplication (ExprIdentifier "loop") (ExprOperations (ArithmeticOp (PlusOp {aL = ExprIdentifier "i", aR = ExprValue (ValInt 1)}))))
                                          (ExprOperations (ArithmeticOp (MulOp {aL = ExprIdentifier "accum", aR = ExprIdentifier "i"})))
                                      )
                                  )
                              )
                              (ExprApplication (ExprApplication (ExprIdentifier "loop") (ExprValue (ValInt 1))) (ExprValue (ValInt 1)))
                          )
                      )
                ]
      let actual = parseProgram "let factorial n = let rec loop i accum = if i > n then accum else loop (i + 1) (accum * i) in loop 1 1"

      expected ~=? actual

-- Utils

parseProgram :: Text -> Maybe Program
parseProgram = parse programP
