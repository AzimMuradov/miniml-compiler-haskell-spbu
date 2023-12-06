module Trees.Common where

import Data.Int (Int64)
import Data.Text (Text)

-- ** Identifier

-- | Any valid identifier (e.g., @he42llo@, @_42@).
type Identifier = Text

data Identifier'
  = Txt Text
  | Gen Int
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Enum, Bounded)

-- | Boolean operator.
data BooleanOperator
  = -- | And operator (@a && b@).
    AndOp
  | -- | Or operator (@a || b@).
    OrOp
  deriving (Show, Eq, Enum, Bounded)

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
  deriving (Show, Eq, Enum, Bounded)

-- | Comparison operator.
data ComparisonOperator
  = -- | Equality operator (@a = b@).
    EqOp
  | -- | Non-equality operator (@a <> b@).
    NeOp
  | -- | Less than operator (@a < b@).
    LtOp
  | -- | Less than or equal operator (@a <= b@).
    LeOp
  | -- | Greater than operator (@a > b@).
    GtOp
  | -- | Greater than or equal operator (@a >= b@).
    GeOp
  deriving (Show, Eq, Enum, Bounded)

-- ** Recursive Functions

type IsRec = Bool

-- ** Types

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
  deriving (Show, Eq, Ord)

-- ** Values

-- | Primitive value.
data Value
  = -- | Unit literal (@()@).
    ValUnit
  | -- | Boolean literal (@true@, @false@).
    ValBool Bool
  | -- | Int literal (e.g., @0@, @4@, @15@, @23@).
    ValInt Int64
  deriving (Show, Eq)
