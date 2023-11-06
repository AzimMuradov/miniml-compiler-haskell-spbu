{-# LANGUAGE OverloadedStrings #-}

module StdLib
  ( stdDeclarationsForAst,
    stdDeclarationsForTypelessAst,
    binOpIdentifier,
    unOpIdentifier,
  )
where

import qualified Parser.Ast as Ast
import qualified Transformations.TypelessAst as TAst

-- * Standard Declarations for 'Ast'

-- | The list of standard declarations for 'Ast'.
stdDeclarationsForAst :: [Ast.StdDeclaration]
stdDeclarationsForAst = [notDecl, printBoolDecl, printIntDecl]

-- ** Declarations

-- | The @not@ function declaration (@not : bool -> bool@).
notDecl :: Ast.StdDeclaration
notDecl = Ast.StdDecl "not" (Ast.TFun Ast.TBool Ast.TBool)

-- | The @print_bool@ function declaration (@print_bool : bool -> unit@).
printBoolDecl :: Ast.StdDeclaration
printBoolDecl = Ast.StdDecl "print_bool" (Ast.TFun Ast.TBool Ast.TUnit)

-- | The @print_int@ function declaration (@print_int : int -> unit@).
printIntDecl :: Ast.StdDeclaration
printIntDecl = Ast.StdDecl "print_int" (Ast.TFun Ast.TInt Ast.TUnit)

-- * Standard Declarations for 'TypelessAst'

-- | The list of standard declarations for 'TypelessAst'.
stdDeclarationsForTypelessAst :: [TAst.Identifier]
stdDeclarationsForTypelessAst = (mapper <$> stdDeclarationsForAst) <> operatorDecls
  where
    mapper (Ast.StdDecl name _) = name

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
