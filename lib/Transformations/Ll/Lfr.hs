module Transformations.Ll.Lfr where

import Trees.Common

data Program = Program [TopLevelDeclaration] IdCnt
  deriving (Show, Eq)

data TopLevelDeclaration
  = TopLevelVarDecl VarDeclaration
  | TopLevelFunDecl Identifier' [Identifier'] Expression
  deriving (Show, Eq)

data VarDeclaration = VarDecl Identifier' Expression
  deriving (Show, Eq)

data Expression
  = ExprId Identifier'
  | ExprVal Value
  | ExprBinOp BinaryOperator Expression Expression
  | ExprUnOp UnaryOperator Expression
  | ExprApp Expression Expression
  | ExprIte Expression Expression Expression
  | ExprLetIn VarDeclaration Expression
  deriving (Show, Eq)
