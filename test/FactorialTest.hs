{-# LANGUAGE OverloadedStrings #-}

module FactorialTest (tests) where

import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import Parser.Ast
import Parser.Parser (parse, programP)
import Test.HUnit (Test (TestList), (~:), (~=?))
import TypeInference.PrettyPrint (pretty)
import TypeInference.Runtime (inferPolytype)

tests :: Test
tests =
  "factorial"
    ~: TestList
      [ testFacRecParsing,
        testFacRecTypeInference,
        testFacRecLoopParsing,
        testFacRecLoopTypeInference
      ]

-- Tests

testFacRecParsing :: Test
testFacRecParsing =
  "recursive factorial parsing"
    ~: do
      let expected =
            Just $
              Program
                [ StmtRecFunDecl $
                    RecFunDecl
                      "factorial"
                      ( Fun
                          (NonEmpty.singleton ("n", Nothing))
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

testFacRecTypeInference :: Test
testFacRecTypeInference =
  "recursive factorial type inference"
    ~: do
      let expected = "int -> int"
      let actual = eval . parseProgram $ "let rec factorial n = if n <= 0 then 1 else n * factorial (n - 1)"

      expected ~=? actual

testFacRecLoopParsing :: Test
testFacRecLoopParsing =
  "recursive factorial with the `loop` nested function parsing"
    ~: do
      let expected =
            Just $
              Program
                [ StmtFunDecl $
                    FunDecl
                      "factorial"
                      ( Fun
                          (NonEmpty.singleton ("n", Nothing))
                          Nothing
                          ( ExprLetRecInF
                              "loop"
                              ( Fun
                                  (NonEmpty.fromList [("i", Nothing), ("accum", Nothing)])
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

testFacRecLoopTypeInference :: Test
testFacRecLoopTypeInference =
  "recursive factorial with the `loop` nested function type inference"
    ~: do
      let expected = "int -> int"
      let actual = eval . parseProgram $ "let factorial n = let rec loop i accum = if i > n then accum else loop (i + 1) (accum * i) in loop 1 1"

      expected ~=? actual

-- Utils

parseProgram :: Text -> Maybe Program
parseProgram = parse programP

eval :: Maybe Program -> String
eval s = case s of
  Nothing -> "Please, try again. Can't parse your program."
  Just p -> case inferPolytype p of
    Left tyerr -> pretty tyerr
    Right ty -> pretty ty
