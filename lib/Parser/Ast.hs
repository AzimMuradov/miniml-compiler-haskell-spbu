-- | Contains all AST elements, all of these produced by the [Parser]("Parser.Parser") module.
module Parser.Ast where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

--------------------------------------------------------Program---------------------------------------------------------

-- * Program

-- | The head of the AST.
newtype Program = Program [Statement]
  deriving (Show, Eq)

-------------------------------------------------------Statements-------------------------------------------------------

-- * Statements

-- | Statement.
data Statement
  = -- | Expression statement, see 'Expression'.
    StmtExpr Expression
  | -- | Variable declaration (e.g., @let x = 5@).
    StmtVarDecl (Identifier, Maybe Type) Expression
  | -- | Function declaration (e.g., @let f x y = x + y@).
    StmtFunDecl Identifier Fun
  | -- | Recursive function declaration (e.g., @let rec f x y = f x 1 + f 1 y@).
    StmtRecFunDecl Identifier Fun
  | -- | Standard library declaration.
    StmtStdDecl Identifier Type
  deriving (Show, Eq)

---------------------------------------------------------Types----------------------------------------------------------

-- * Types

-- | All existing types.
data Type
  = -- | Unit type
    TUnit
  | -- | Boolean type.
    TBool
  | -- | Integer type.
    TInt
  | -- | Function type.
    --
    -- It contains the type of the first parameter and the result of the function (e.g., @int -> (int -> bool -> bool)@).
    TFun Type Type
  deriving (Show, Eq)

------------------------------------------------------Expressions-------------------------------------------------------

-- * Expressions

-- | Expression.
data Expression
  = -- | Identifier expression, see 'Identifier'.
    ExprIdentifier Identifier
  | -- | Value expression, see 'Value'.
    ExprValue Value
  | -- | Binary operation, see 'BinaryOperator'.
    ExprBinaryOperation BinaryOperator Expression Expression
  | -- | Value expression, see 'UnaryOperator'.
    ExprUnaryOperation UnaryOperator Expression
  | -- | ( f 6, (fun x y = x + y) 5 6 )
    ExprApplication Expression Expression
  | -- | If-else expression.
    --
    -- > if condition then expr1 else expr2
    ExprIf Expression Expression Expression
  | -- | ( let x = 4 in ... )
    ExprLetInV (Identifier, Maybe Type) Expression Expression
  | -- | ( let f x y = x + y in ... )
    ExprLetInF Identifier Fun Expression
  | -- | ( let rec f x y = x + y in ... )
    ExprLetRecInF Identifier Fun Expression
  deriving (Show, Eq)

-- ** Values

-- | Literal or function value.
data Value
  = -- | Unit value (e.g. @()@).
    ValUnit 
  | -- | Boolean literal value (e.g., @true@, @false@).
    ValBool Bool
  | -- | Int literal value (e.g., @4@, @-15@).
    ValInt Integer
  | -- | Function value, see 'Fun'.
    ValFun Fun
  deriving (Show, Eq)

-- | Function representation without name.
--
-- It contains its parameters, returned type and body.
--
-- > fun x -> true
--
-- > fun x y -> x + y
data Fun = Fun (NonEmpty (Identifier, Maybe Type)) (Maybe Type) Expression
  deriving (Show, Eq)

-- ** Operators

-- | Binary operator.
data BinaryOperator
  = -- | Boolean operator, see 'BooleanOperator'.
    BooleanOp BooleanOperator
  | -- | Arithmetic operator, see 'ArithmeticOperator'.
    ArithmeticOp ArithmeticOperator
  | -- | Comparison operator, see 'ComparisonOperator'.
    ComparisonOp ComparisonOperator
  deriving (Show, Eq)

-- | Unary operator.
data UnaryOperator
  = -- | Unary minus operator (@-a@), works only for @int@.
    UnaryMinusOp
  deriving (Show, Eq)

-- | Boolean operator.
data BooleanOperator
  = -- | And operator (@a && b@).
    AndOp
  | -- | Or operator (@a || b@).
    OrOp
  deriving (Show, Eq)

-- | Arithmetic operator.
data ArithmeticOperator
  = -- | Addition operator (@a + b@).
    PlusOp
  | -- | Subtraction operator (@a - b@).
    MinusOp
  | -- | Multiplication operator (@a * b@).
    MulOp
  | -- | Division operator (@a / b@).
    DivOp
  deriving (Show, Eq)

-- | Comparison operator.
data ComparisonOperator
  = -- | Equality check operator (@a = b@).
    EqOp
  | -- | Non-equality check operator (@a <> b@).
    NeOp
  | -- | Less than operator (@a < b@).
    LtOp
  | -- | Less than or equal operator (@a <= b@).
    LeOp
  | -- | More than operator (@a > b@).
    MtOp
  | -- | More than or equal operator (@a >= b@).
    MeOp
  deriving (Show, Eq)

-- ** Identifier

-- | Any valid identifier (e.g., @he42llo@, @_42@).
type Identifier = Text
