{-# LANGUAGE OverloadedStrings #-}

module TypeInference.TypeInferenceTest (tests) where

import Data.Text (Text)
import Parser.Ast
import Parser.Parser (parse, programP)
import Test.HUnit (Test (TestList), (~:), (~=?))
import TypeInference.PrettyPrint (pretty)
import TypeInference.Runtime (inferPolytype)

tests :: Test
tests =
  "type-inference"
    ~: TestList
      [ test0,
        test1,
        test2,
        test3,
        test4,
        test5,
        test6,
        test7,
        test8,
        test9,
        test10,
        test11,
        test12,
        test13,
        testredecalaration,
        testfac,
        testfib,
        testrec,
        testfix0,
        testfix1,
        testfix2
      ]

-- Tests

test0 :: Test
test0 =
  "[1 + 1]"
    ~: do
      let expected = "int"
      let actual = eval $ parseProgram "1 + 1"

      expected ~=? actual

test1 :: Test
test1 =
  "[false = true]"
    ~: do
      let expected = "bool"
      let actual = eval $ parseProgram "false = true"

      expected ~=? actual

test2 :: Test
test2 =
  "[false = 1]"
    ~: do
      let expected = "The type 'bool' does not match the type 'int'"
      let actual = eval $ parseProgram "false = 1"

      expected ~=? actual

test3 :: Test
test3 =
  "[let x = 1]"
    ~: do
      let expected = "int"
      let actual = eval $ parseProgram "let x = 1"

      expected ~=? actual

test4 :: Test
test4 =
  "[let f x = x + 1]"
    ~: do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let f x = x + 1"

      expected ~=? actual

test5 :: Test
test5 =
  "[let f x = x + true]"
    ~: do
      let expected = "It is not possible to apply this operation between 'u0' and 'bool'"
      let actual = eval $ parseProgram "let f x = x + true"

      expected ~=? actual

test6 :: Test
test6 =
  "[let rec f x = x + 1]"
    ~: do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let rec f x = x + 1"

      expected ~=? actual

test7 :: Test
test7 =
  "[let x = 1 in x + 1]"
    ~: do
      let expected = "int"
      let actual = eval $ parseProgram "let x = 1 in x + 1"

      expected ~=? actual

test8 :: Test
test8 =
  "[let rec f x = true in f 5 <> false]"
    ~: do
      let expected = "bool"
      let actual = eval $ parseProgram "let rec f x = true in f 5 <> false"

      expected ~=? actual

test9 :: Test
test9 =
  "[fun x y -> x + y]"
    ~: do
      let expected = "int -> int -> int"
      let actual = eval $ parseProgram "fun x y -> x + y"

      expected ~=? actual

test10 :: Test
test10 =
  "[let x = 1]"
    ~: do
      let expected = "int"
      let actual = eval $ parseProgram "let x = 1"

      expected ~=? actual

test11 :: Test
test11 =
  "[let f x = y + x]"
    ~: do
      let expected = "Unbound variable 'y'"
      let actual = eval $ parseProgram "let f x = y + x"

      expected ~=? actual

test12 :: Test
test12 =
  "[let f x = f (x - 1)]"
    ~: do
      let expected = "Unbound variable 'f'"
      let actual = eval $ parseProgram "let f x = f (x - 1)"

      expected ~=? actual

test13 :: Test
test13 =
  "[1 && (false || true)]"
    ~: do
      let expected = "It is not possible to apply this operation between 'int' and 'bool'"
      let actual = eval $ parseProgram "1 && (false || true)"

      expected ~=? actual

testredecalaration :: Test
testredecalaration =
  "[let x = true;; let f x = x + 1]"
    ~: do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let x = true;; let f x = x + 1"

      expected ~=? actual

testfac :: Test
testfac =
  "[let rec fact n = if n <= 1 then 1 else n * fact (n - 1)]"
    ~: do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)"

      expected ~=? actual

testfib :: Test
testfib =
  "[let n = 30;; let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);; fib (n - 2)]"
    ~: do
      let expected = "int"
      let actual = eval $ parseProgram "let n = 30;; let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);; fib (n - 2)"

      expected ~=? actual

testrec :: Test
testrec =
  "[let rec f x = if x = 0 then 0 else let g x = if x = 0 then 0 else f (x - 1) in g (x - 1)]"
    ~: do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let rec f x = if x = 0 then 0 else let g x = if x = 0 then 0 else f (x - 1) in g (x - 1)"

      expected ~=? actual

testfix0 :: Test
testfix0 =
  "[let rec fix f = f (fix f)]"
    ~: do
      let expected = "forall a6. (a6 -> a6) -> a6"
      let actual = eval $ parseProgram "let rec fix f = f (fix f)"

      expected ~=? actual

testfix1 :: Test
testfix1 =
  "[let rec fix f x = f (fix f) x;; fix (fun re n -> if n <= 1 then 1 else n * re (n - 1))]"
    ~: do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let rec fix f x = f (fix f) x;; fix (fun re n -> if n <= 1 then 1 else n * re (n - 1))"

      expected ~=? actual

testfix2 :: Test
testfix2 =
  "[let rec fix f x = f (fix f) x;; fix (fun re n -> if n <= 1 then 1 else n * re (n - 1)) 10]"
    ~: do
      let expected = "int"
      let actual = eval $ parseProgram "let rec fix f x = f (fix f) x;; fix (fun re n -> if n <= 1 then 1 else n * re (n - 1)) 10"

      expected ~=? actual

-- Utils

parseProgram :: Text -> Maybe Program
parseProgram = parse programP

eval :: Maybe Program -> String
eval s = case s of
  Nothing -> "Please, try again. Can't parse your program."
  Just (Program p) -> case inferPolytype p of
    Left tyerr -> pretty tyerr
    Right ty -> pretty ty
