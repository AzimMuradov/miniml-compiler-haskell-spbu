module Transformations.Anf.Anf where

import Data.Int (Int64)
import Trees.Common

newtype Program = Program [GlobalDeclaration]
  deriving (Show, Eq)

data GlobalDeclaration
  = GlobVarDecl Identifier' Expression
  | GlobFunDecl Identifier' [Identifier'] Expression
  deriving (Show, Eq)

data Expression
  = ExprAtom AtomicExpression
  | ExprComp ComplexExpression
  | ExprLetIn (Identifier', Expression) Expression
  deriving (Show, Eq)

data AtomicExpression
  = AtomId Identifier'
  | AtomUnit
  | AtomBool Bool
  | AtomInt Int64
  | AtomBinOp BinaryOperator AtomicExpression AtomicExpression
  | AtomUnOp UnaryOperator AtomicExpression
  deriving (Show, Eq)

data ComplexExpression
  = CompApp Identifier' AtomicExpression
  | CompIte AtomicExpression Expression Expression
  deriving (Show, Eq)
