module Trees.Common where

import Data.Int (Int64)
import Data.Text (Text)

-- * Common Components of Program Representations

-- ** Identifier

type Identifier = Text

data Identifier'
  = Txt Identifier
  | Gen IdCnt Identifier
  deriving (Show, Eq, Ord)

type IdCnt = Int

-- ** Operators

-- | Binary operator.
data BinaryOperator
  = -- | Boolean operator, see 'BooleanOperator'.
    BoolOp BooleanOperator
  | -- | Arithmetic operator, see 'ArithmeticOperator'.
    ArithOp ArithmeticOperator
  | -- | Comparison operator, see 'ComparisonOperator'.
    CompOp ComparisonOperator
  deriving (Show, Eq)

-- | Unary operator.
data UnaryOperator
  = -- | Unary minus operator (@-a@), works only for @int@.
    UnMinusOp
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
    -- It contains the type of the first parameter and the result of the function
    -- (e.g., @int -> (int -> bool -> bool)@).
    TFun Type Type
  deriving (Show, Eq, Ord)

-- | Arity of a function.
type Arity = Int

-- ** Values

-- | Primitive value.
data PrimitiveValue
  = -- | Unit value (@()@).
    PrimValUnit
  | -- | Boolean value (@true@, @false@).
    PrimValBool Bool
  | -- | Int value (e.g., @0@, @4@, @15@, @23@).
    PrimValInt Int64
  deriving (Show, Eq)
