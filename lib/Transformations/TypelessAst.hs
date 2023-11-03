module Transformations.TypelessAst where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

newtype Program = Program [Statement]
  deriving (Show, Eq)

data Statement
  = StmtDecl Identifier Expression
  | StmtExpr Expression
  deriving (Show, Eq)

data Expression
  = ExprIdentifier Identifier
  | ExprValue Value
  | ExprApplication Expression (NonEmpty Expression)
  | ExprIte Expression Expression Expression
  | ExprLetIn Identifier Expression Expression
  deriving (Show, Eq)

data Value
  = ValUnit
  | ValBool Bool
  | ValInt Integer
  | ValFun (NonEmpty Identifier) Expression
  deriving (Show, Eq)

type Identifier = Text
