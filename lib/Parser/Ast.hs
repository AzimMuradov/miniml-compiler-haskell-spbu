-- |
-- Module      : Ast
-- Description : Contains all AST elements.
--
-- Contains all AST elements, all of these produced by the [Parser]("Parser.Parser") module.
module Parser.Ast where

import Data.List.NonEmpty (NonEmpty)
import Trees.Common

--------------------------------------------------------Program---------------------------------------------------------

-- * Program

-- | The head of the AST.
newtype Program = Program [Statement]
  deriving (Show, Eq)

-- ** Statements

-- | Statement.
data Statement
  = -- | Declaration statement, see 'Declaration'.
    StmtDecl Declaration
  | -- | Expression statement, see 'Expression'.
    StmtExpr Expression
  deriving (Show, Eq)

------------------------------------------------------Declarations------------------------------------------------------

-- * Declarations

-- | Declaration.
data Declaration
  = -- | Variable declaration.
    --
    -- > let x = 5
    DeclVar (Identifier, Maybe Type) Expression
  | -- | Function declaration.
    --
    -- > let f x y = x + y
    --
    -- > let rec f x y = f x 1 + f 1 y
    DeclFun Identifier IsRec Fun
  deriving (Show, Eq)

------------------------------------------------------Expressions-------------------------------------------------------

-- * Expressions

-- | Expression.
data Expression
  = -- | Identifier expression, see 'Identifier'.
    ExprId Identifier
  | -- | Primitive value expression, see 'PrimitiveValue'.
    ExprPrimVal PrimitiveValue
  | -- | Binary operation, see 'BinaryOperator'.
    ExprBinOp BinaryOperator Expression Expression
  | -- | Unary operation, see 'UnaryOperator'.
    ExprUnOp UnaryOperator Expression
  | -- | Function application expression.
    --
    -- > f 6
    --
    -- > (fun x y = x + y) 5
    ExprApp Expression Expression
  | -- | If-then-else expression.
    --
    -- > if x > 4 then x * 8 else x / 15
    ExprIte Expression Expression Expression
  | -- | Let expression.
    --
    -- > let x = 4 in x * x
    --
    -- > let f x y = x + y in f 4 8
    --
    -- > let rec f x y = f x 1 + f 1 y in f 4 8
    ExprLetIn Declaration Expression
  | -- | Anonymous function, see 'Fun'.
    ExprFun Fun
  deriving (Show, Eq)

-- | Function representation without the name.
--
-- It contains its parameters, returned type and body.
--
-- > fun x -> true
--
-- > fun x y -> x + y
data Fun = Fun (NonEmpty (Identifier, Maybe Type)) (Maybe Type) Expression
  deriving (Show, Eq)
