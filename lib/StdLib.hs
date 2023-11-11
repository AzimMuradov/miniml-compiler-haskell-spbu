{-# LANGUAGE OverloadedStrings #-}

module StdLib
  ( TypedStdDeclaration,
    typedStdDeclarations,
    stdDeclarationsForTypelessAst,
    binOpIdentifier,
    unOpIdentifier,
  )
where

import Data.Text (Text)
import qualified Parser.Ast as Ast
import qualified Transformations.TypelessAst as TAst

-- * Standard Declarations for 'Ast'

type Identifier = Text

type TypedStdDeclaration = (Identifier, Ast.Type)

-- | The list of standard declarations for 'Ast'.
typedStdDeclarations :: [TypedStdDeclaration]
typedStdDeclarations = [notDecl, printBoolDecl, printIntDecl]

-- ** Declarations

-- | The @not@ function declaration (@not : bool -> bool@).
notDecl :: TypedStdDeclaration
notDecl = ("not", Ast.TFun Ast.TBool Ast.TBool)

-- | The @print_bool@ function declaration (@print_bool : bool -> unit@).
printBoolDecl :: TypedStdDeclaration
printBoolDecl = ("print_bool", Ast.TFun Ast.TBool Ast.TUnit)

-- | The @print_int@ function declaration (@print_int : int -> unit@).
printIntDecl :: TypedStdDeclaration
printIntDecl = ("print_int", Ast.TFun Ast.TInt Ast.TUnit)

-- * Standard Declarations for 'TypelessAst'

-- | The list of standard declarations for 'TypelessAst'.
stdDeclarationsForTypelessAst :: [TAst.Identifier]
stdDeclarationsForTypelessAst = (mapper <$> typedStdDeclarations) <> operatorDecls
  where
    mapper (name, _) = name

    operatorDecls = (binOpIdentifier <$> binOps) <> (unOpIdentifier <$> unaryOps)

    binOps =
      (Ast.BooleanOp <$> [minBound .. maxBound])
        <> (Ast.ArithmeticOp <$> [minBound .. maxBound])
        <> (Ast.ComparisonOp <$> [minBound .. maxBound])
    unaryOps = [minBound .. maxBound]

-- ** Operator Declarations

binOpIdentifier :: Ast.BinaryOperator -> TAst.Identifier
binOpIdentifier (Ast.BooleanOp Ast.AndOp) = "(&&)"
binOpIdentifier (Ast.BooleanOp Ast.OrOp) = "(||)"
binOpIdentifier (Ast.ArithmeticOp Ast.PlusOp) = "(+)"
binOpIdentifier (Ast.ArithmeticOp Ast.MinusOp) = "(-)"
binOpIdentifier (Ast.ArithmeticOp Ast.MulOp) = "(*)"
binOpIdentifier (Ast.ArithmeticOp Ast.DivOp) = "(/)"
binOpIdentifier (Ast.ComparisonOp Ast.EqOp) = "(=)"
binOpIdentifier (Ast.ComparisonOp Ast.NeOp) = "(<>)"
binOpIdentifier (Ast.ComparisonOp Ast.LtOp) = "(<)"
binOpIdentifier (Ast.ComparisonOp Ast.LeOp) = "(<=)"
binOpIdentifier (Ast.ComparisonOp Ast.GtOp) = "(>)"
binOpIdentifier (Ast.ComparisonOp Ast.GeOp) = "(>=)"

unOpIdentifier :: Ast.UnaryOperator -> TAst.Identifier
unOpIdentifier Ast.UnaryMinusOp = "(~-)"
