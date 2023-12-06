module Transformations.Simplification.SimplifiedAst where

import Data.List.NonEmpty (NonEmpty)
import Trees.Common

data Program = Program [Declaration] IdCnt
  deriving (Show, Eq)

data Declaration
  = DeclVar Identifier' Expression
  | DeclFun Identifier' IsRec Fun
  deriving (Show, Eq)

data Expression
  = ExprId Identifier'
  | ExprVal Value
  | ExprBinOp BinaryOperator Expression Expression
  | ExprUnOp UnaryOperator Expression
  | ExprApp Expression Expression
  | ExprIte Expression Expression Expression
  | ExprLetIn Declaration Expression
  | ExprFun Fun
  deriving (Show, Eq)

data Fun = Fun (NonEmpty Identifier') Expression
  deriving (Show, Eq)
