{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StdLib
  ( TypedStdDeclaration,
    typedStdDeclarations,
    stdDeclarations,
    binOpIdentifier,
    unOpIdentifier,
  )
where

import Data.Text (Text)
import Trees.Common

-- * Standard Library

type TypedStdDeclaration = (Identifier, Type)

-- | The list of typed standard declarations (but without the operators).
typedStdDeclarations :: [TypedStdDeclaration]
typedStdDeclarations = [notDecl, printBoolDecl, printIntDecl]

-- | The list of standard declarations.
stdDeclarations :: [Identifier]
stdDeclarations = (mapper <$> typedStdDeclarations) <> operatorDecls
  where
    mapper (name, _) = name

    operatorDecls = (binOpIdentifier <$> binOps) <> (unOpIdentifier <$> unaryOps)

    binOps =
      (BooleanOp <$> [minBound .. maxBound])
        <> (ArithmeticOp <$> [minBound .. maxBound])
        <> (ComparisonOp <$> [minBound .. maxBound])
    unaryOps = [minBound .. maxBound]

-- ** Function Declarations

-- | The @not@ function declaration (@not : bool -> bool@).
notDecl :: TypedStdDeclaration
notDecl = ("not", TFun TBool TBool)

-- | The @print_bool@ function declaration (@print_bool : bool -> unit@).
printBoolDecl :: TypedStdDeclaration
printBoolDecl = ("print_bool", TFun TBool TUnit)

-- | The @print_int@ function declaration (@print_int : int -> unit@).
printIntDecl :: TypedStdDeclaration
printIntDecl = ("print_int", TFun TInt TUnit)

-- ** Operator Declarations

binOpIdentifier :: BinaryOperator -> Identifier
binOpIdentifier (BooleanOp AndOp) = "&&"
binOpIdentifier (BooleanOp OrOp) = "||"
binOpIdentifier (ArithmeticOp PlusOp) = "+"
binOpIdentifier (ArithmeticOp MinusOp) = "-"
binOpIdentifier (ArithmeticOp MulOp) = "*"
binOpIdentifier (ArithmeticOp DivOp) = "/"
binOpIdentifier (ComparisonOp EqOp) = "="
binOpIdentifier (ComparisonOp NeOp) = "<>"
binOpIdentifier (ComparisonOp LtOp) = "<"
binOpIdentifier (ComparisonOp LeOp) = "<="
binOpIdentifier (ComparisonOp GtOp) = ">"
binOpIdentifier (ComparisonOp GeOp) = ">="

unOpIdentifier :: UnaryOperator -> Identifier
unOpIdentifier UnaryMinusOp = "-"

data StdLibDecl
  = And
  | Or
  | Not
  | UnMinus
  | Plus
  | Minus
  | Mul
  | Div
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | PrintBool
  | PrintInt

stdDeclName :: StdLibDecl -> Text
stdDeclName = \case
  And -> "(&&)"
  Or -> "(||)"
  Not -> "not"
  UnMinus -> "(~-)"
  Plus -> "(+)"
  Minus -> "(-)"
  Mul -> "(*)"
  Div -> "(/)"
  Eq -> "(=)"
  Ne -> "(<>)"
  Lt -> "(<)"
  Le -> "(<=)"
  Gt -> "(>)"
  Ge -> "(>=)"
  PrintBool -> "print_bool"
  PrintInt -> "print_int"
