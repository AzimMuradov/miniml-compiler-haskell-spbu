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
      (BoolOp <$> [minBound .. maxBound])
        <> (ArithOp <$> [minBound .. maxBound])
        <> (CompOp <$> [minBound .. maxBound])
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
binOpIdentifier (BoolOp AndOp) = "(&&)"
binOpIdentifier (BoolOp OrOp) = "(||)"
binOpIdentifier (ArithOp PlusOp) = "(+)"
binOpIdentifier (ArithOp MinusOp) = "(-)"
binOpIdentifier (ArithOp MulOp) = "(*)"
binOpIdentifier (ArithOp DivOp) = "(/)"
binOpIdentifier (CompOp EqOp) = "(=)"
binOpIdentifier (CompOp NeOp) = "(<>)"
binOpIdentifier (CompOp LtOp) = "(<)"
binOpIdentifier (CompOp LeOp) = "(<=)"
binOpIdentifier (CompOp GtOp) = "(>)"
binOpIdentifier (CompOp GeOp) = "(>=)"

unOpIdentifier :: UnaryOperator -> Identifier
unOpIdentifier UnMinusOp = "(~-)"

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
