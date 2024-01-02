module Transformations.Ll.Lfr where

import Trees.Common

data Program = Program [GlobalDeclaration] IdCnt
  deriving (Show, Eq)

data GlobalDeclaration
  = GlobVarDecl VarDeclaration
  | GlobFunDecl Identifier' [Identifier'] Expression
  deriving (Show, Eq)

data VarDeclaration = VarDecl Identifier' Expression
  deriving (Show, Eq)

data Expression
  = ExprId Identifier'
  | ExprVal PrimitiveValue
  | ExprBinOp BinaryOperator Expression Expression
  | ExprUnOp UnaryOperator Expression
  | ExprApp Expression Expression
  | ExprIte Expression Expression Expression
  | ExprLetIn VarDeclaration Expression
  deriving (Show, Eq)
