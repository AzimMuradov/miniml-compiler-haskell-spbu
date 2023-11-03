module Transformations.Anf where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

newtype Program = Program [Statement]
  deriving (Show, Eq)

data Statement
  = StmtDecl Identifier Expression
  | StmtExpr Expression
  deriving (Show, Eq)

data AtomicExpression
  = AtomExprIdentifier Identifier
  | AtomExprUnit
  | AtomExprBool Bool
  | AtomExprInt Integer
  | AtomExprClosure (NonEmpty Identifier) Expression
  deriving (Show, Eq)

data ComplexExpression
  = CompExprApp AtomicExpression (NonEmpty AtomicExpression)
  | CompExprIte AtomicExpression Expression Expression
  deriving (Show, Eq)

data Expression
  = ExprAtomExpr AtomicExpression
  | ExprCompExpr ComplexExpression
  | ExprLetIn Identifier Expression Expression
  deriving (Show, Eq)

type Identifier = Text
