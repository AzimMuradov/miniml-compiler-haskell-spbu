-- | Contains all AST elements, all of these produced by the [Parser]("Parser.Parser") module.
module Parser.Ast where

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
  | -- | Variable declaration statement, see 'VarDecl'.
    StmtVarDecl VarDecl
  | -- | Function declaration statement, see 'FunDecl'.
    StmtFunDecl FunDecl
  | -- | Recursive function declaration statement, see 'RecFunDecl'.
    StmtRecFunDecl RecFunDecl
  deriving (Show, Eq)

-- ** Declarations

-- TODO : Add docs ( let x = 5 )
data VarDecl = VarDecl (Identifier, Maybe Type) Expression
  deriving (Show, Eq)

-- TODO : Add docs ( let f x y = x + y )
data FunDecl = FunDecl Identifier Fun
  deriving (Show, Eq)

-- TODO : Add docs ( let rec f x y = f x 1 + f 1 y)
data RecFunDecl = RecFunDecl Identifier Fun
  deriving (Show, Eq)

---------------------------------------------------------Types----------------------------------------------------------

-- * Types

-- | All existing types.
data Type
  = -- | Boolean type.
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
  | -- | Operation expression, see 'Operations'.
    ExprOperations Operations
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
  = -- | Boolean literal value (e.g., @true@, @false@).
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
data Fun = Fun [(Identifier, Maybe Type)] (Maybe Type) Expression
  deriving (Show, Eq)

-- ** Operations

-- | Operation.
data Operations
  = -- | Boolean operation, see 'BooleanOp'.
    BooleanOp BooleanOp
  | -- | Negation operation (@not a@), works only for @bool@.
    NotOp Expression
  | -- | Arithmetic operation, see 'ArithmeticOp'.
    ArithmeticOp ArithmeticOp
  | -- | Comparison operation, see 'ComparisonOp'.
    ComparisonOp ComparisonOp
  deriving (Show, Eq)

-- | Boolean operation.
data BooleanOp
  = -- | And operation (@a && b@).
    AndOp {bL :: Expression, bR :: Expression}
  | -- | Or operation (@a || b@).
    OrOp {bL :: Expression, bR :: Expression}
  deriving (Show, Eq)

-- | Arithmetic operation.
data ArithmeticOp
  = -- | Addition operation (@a + b@).
    PlusOp {aL :: Expression, aR :: Expression}
  | -- | Subtraction operation (@a - b@).
    MinusOp {aL :: Expression, aR :: Expression}
  | -- | Multiplication operation (@a * b@).
    MulOp {aL :: Expression, aR :: Expression}
  | -- | Division operation (@a / b@).
    DivOp {aL :: Expression, aR :: Expression}
  deriving (Show, Eq)

-- | Comparison operation.
data ComparisonOp
  = -- | Equality check (@a = b@).
    EqOp {cL :: Expression, cR :: Expression}
  | -- | Non-equality check (@a <> b@).
    NeOp {cL :: Expression, cR :: Expression}
  | -- | Less than (@a < b@).
    LtOp {cL :: Expression, cR :: Expression}
  | -- | Less than or equal operator (@a <= b@).
    LeOp {cL :: Expression, cR :: Expression}
  | -- | More than operator (@a > b@).
    MtOp {cL :: Expression, cR :: Expression}
  | -- | More than or equal operator (@a >= b@).
    MeOp {cL :: Expression, cR :: Expression}
  deriving (Show, Eq)

-- ** Identifier

-- | Any valid identifier (e.g., @he42llo@, @_42@).
type Identifier = Text
