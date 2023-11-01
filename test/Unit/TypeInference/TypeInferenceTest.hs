{-# LANGUAGE OverloadedStrings #-}

module Unit.TypeInference.TypeInferenceTest (tests) where

import Parser.Ast
import Parser.Parser (parseProgram)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import TypeInference.PrettyPrint (pretty)
import TypeInference.Runtime (inferPolytype)

tests :: TestTree
tests =
  testGroup
    "type-inference"
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
      testfib,
      testrec,
      testfix0,
      testfix1,
      testfix2,
      testduplicate0,
      testduplicate1,
      testduplicate2
    ]

-- Tests

test0 :: TestTree
test0 =
  testCase "[1 + 1]" $ do
    let expected = "int"
    let actual = eval $ parseProgram "1 + 1"

    expected @=? actual

test1 :: TestTree
test1 =
  testCase "[false = true]" $
    do
      let expected = "bool"
      let actual = eval $ parseProgram "false = true"

      expected @=? actual

test2 :: TestTree
test2 =
  testCase
    "[false = 1]"
    $ do
      let expected = "It is not possible to apply this operation between 'bool' and 'int'"
      let actual = eval $ parseProgram "false = 1"

      expected @=? actual

test3 :: TestTree
test3 =
  testCase
    "[let x = 1]"
    $ do
      let expected = "int"
      let actual = eval $ parseProgram "let x = 1"

      expected @=? actual

test4 :: TestTree
test4 =
  testCase
    "[let f x = x + 1]"
    $ do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let f x = x + 1"

      expected @=? actual

test5 :: TestTree
test5 =
  testCase
    "[let f x = x + true]"
    $ do
      let expected = "It is not possible to apply this operation between 'u0' and 'bool'"
      let actual = eval $ parseProgram "let f x = x + true"

      expected @=? actual

test6 :: TestTree
test6 =
  testCase
    "[let rec f x = x + 1]"
    $ do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let rec f x = x + 1"

      expected @=? actual

test7 :: TestTree
test7 =
  testCase
    "[let x = 1 in x + 1]"
    $ do
      let expected = "int"
      let actual = eval $ parseProgram "let x = 1 in x + 1"

      expected @=? actual

test8 :: TestTree
test8 =
  testCase
    "[let rec f x = true in f 5 <> false]"
    $ do
      let expected = "bool"
      let actual = eval $ parseProgram "let rec f x = true in f 5 <> false"

      expected @=? actual

test9 :: TestTree
test9 =
  testCase
    "[fun x y -> x + y]"
    $ do
      let expected = "int -> int -> int"
      let actual = eval $ parseProgram "fun x y -> x + y"

      expected @=? actual

test10 :: TestTree
test10 =
  testCase
    "[let x = 1]"
    $ do
      let expected = "int"
      let actual = eval $ parseProgram "let x = 1"

      expected @=? actual

test11 :: TestTree
test11 =
  testCase
    "[let f x = y + x]"
    $ do
      let expected = "Unbound variable 'y'"
      let actual = eval $ parseProgram "let f x = y + x"

      expected @=? actual

test12 :: TestTree
test12 =
  testCase
    "[let f x = f (x - 1)]"
    $ do
      let expected = "Unbound variable 'f'"
      let actual = eval $ parseProgram "let f x = f (x - 1)"

      expected @=? actual

test13 :: TestTree
test13 =
  testCase
    "[1 && (false || true)]"
    $ do
      let expected = "It is not possible to apply this operation between 'int' and 'bool'"
      let actual = eval $ parseProgram "1 && (false || true)"

      expected @=? actual

testredecalaration :: TestTree
testredecalaration =
  testCase
    "[let x = true;; let f x = x + 1]"
    $ do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let x = true;; let f x = x + 1"

      expected @=? actual

testfib :: TestTree
testfib =
  testCase
    "[let n = 30;; let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);; fib (n - 2)]"
    $ do
      let expected = "int"
      let actual = eval $ parseProgram "let n = 30;; let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);; fib (n - 2)"

      expected @=? actual

testrec :: TestTree
testrec =
  testCase
    "[let rec f x = if x = 0 then 0 else let g x = if x = 0 then 0 else f (x - 1) in g (x - 1)]"
    $ do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let rec f x = if x = 0 then 0 else let g x = if x = 0 then 0 else f (x - 1) in g (x - 1)"

      expected @=? actual

testduplicate0 :: TestTree
testduplicate0 =
  testCase
    "[let f = 5 let f = 6]"
    $ do
      let expected = "int"
      let actual = eval $ parseProgram "let f = 5 let f = 6"

      expected @=? actual

testduplicate1 :: TestTree
testduplicate1 =
  testCase
    "[let f = 5 let f = fun x y -> x + y]"
    $ do
      let expected = "int -> int -> int"
      let actual = eval $ parseProgram "let f = 5 let f = fun x y -> x + y"

      expected @=? actual

testduplicate2 :: TestTree
testduplicate2 =
  testCase
    "[let f = 5 let f = fun x -> x = 2 let k = f (fun x -> x + 1)]"
    $ do
      let expected = "The type 'int' does not match the type 'u1 -> u1'"
      let actual = eval $ parseProgram "let f = 5 let f = fun x -> x = 2 let k = f (fun x -> x + 1)"

      expected @=? actual

testfix0 :: TestTree
testfix0 =
  testCase
    "[let rec fix f = f (fix f)]"
    $ do
      let expected = "forall a6. (a6 -> a6) -> a6"
      let actual = eval $ parseProgram "let rec fix f = f (fix f)"

      expected @=? actual

testfix1 :: TestTree
testfix1 =
  testCase
    "[let rec fix f x = f (fix f) x;; fix (fun re n -> if n <= 1 then 1 else n * re (n - 1))]"
    $ do
      let expected = "int -> int"
      let actual = eval $ parseProgram "let rec fix f x = f (fix f) x;; fix (fun re n -> if n <= 1 then 1 else n * re (n - 1))"

      expected @=? actual

testfix2 :: TestTree
testfix2 =
  testCase
    "[let rec fix f x = f (fix f) x;; fix (fun re n -> if n <= 1 then 1 else n * re (n - 1)) 10]"
    $ do
      let expected = "int"
      let actual = eval $ parseProgram "let rec fix f x = f (fix f) x;; fix (fun re n -> if n <= 1 then 1 else n * re (n - 1)) 10"

      expected @=? actual

-- Utils

eval :: Maybe Program -> String
eval s = case s of
  Nothing -> "Please, try again. Can't parse your program."
  Just p -> case inferPolytype p of
    Left tyerr -> pretty tyerr
    Right ty -> pretty ty
