module CodeGen.Tast where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

newtype Program = Program [Statement]
  deriving (Show, Eq)

data Statement
  = StmtDecl Identifier Expression
  | StmtExpr Expression
  deriving (Show, Eq)

data AtomicExpression
  = AtomIdentifier Identifier
  | AtomUnit
  | AtomBool Bool
  | AtomInt Integer
  | AtomClosure (NonEmpty Identifier) Expression
  deriving (Show, Eq)

data ComplexExpression
  = CompApp AtomicExpression (NonEmpty AtomicExpression)
  | CompIte AtomicExpression Expression Expression
  deriving (Show, Eq)

data Expression
  = ExprAtom AtomicExpression
  | ExprComp ComplexExpression
  | ExprLetIn Identifier Expression Expression
  deriving (Show, Eq)

type Identifier = Text
