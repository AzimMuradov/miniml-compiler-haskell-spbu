-- | Contains all AST elements, all of these produced by the [Parser]("Parser.Parser") module.
module Parser.Ast where

import Data.Text (Text)
import MaybeVoid (MaybeVoid)

--------------------------------------------------------Program---------------------------------------------------------

-- * Program

-- | The head of the AST.
data Program = Program
  { -- | Top level variable declarations.
    topLevelVarDecls :: [VarDecl],
    -- | Top level function definitions.
    topLevelFunctionDefs :: [FunctionDef]
  }
  deriving (Show)

-- | Function definition.
data FunctionDef = FunctionDef {funcName :: Identifier, func :: Function}
  deriving (Show)

------------------------------------------------------Expressions-------------------------------------------------------

-- * Expressions

-- | Expression.
data Expression
  = -- | Value expression, see 'Value'.
    ExprValue Value
  | -- | Identifier expression, see 'Identifier'.
    ExprIdentifier Identifier
  | -- | Unary operation expression (e.g., @!x@, @-4@), see 'UnaryOp'.
    ExprUnaryOp UnaryOp Expression
  | -- | Binary operation expression (e.g., @x + 7@), see 'BinaryOp'.
    ExprBinaryOp BinaryOp Expression Expression
  | -- | Array access by index expression.
    --
    -- > a[3]
    --
    -- > // func foo() int
    -- > ([2] int {3, 5})[1 + foo()]
    ExprArrayAccessByIndex Expression Expression
  | -- | Function call expression.
    --
    -- > foo(17, x, bar())
    --
    -- > (func (x int) int { return x * x; })(3)
    ExprFuncCall Expression [Expression]
  deriving (Show)

-- ** Operators

-- | Binary operators.
data BinaryOp
  = -- | Or operator (@a || b@), works only for @bool@.
    OrOp
  | -- | And operator (@a && b@), works only for @bool@.
    AndOp
  | -- | Equality operator (@a == b@).
    EqOp
  | -- | Inequality operator (@a != b@).
    NeOp
  | -- | Less than or equal operator (@a <= b@), works only for @int@ and @string@.
    LeOp
  | -- | Less than operator (@a < b@), works only for @int@ and @string@.
    LtOp
  | -- | More than or equal operator (@a >= b@), works only for @int@ and @string@.
    MeOp
  | -- | More than operator (@a > b@), works only for @int@ and @string@.
    MtOp
  | -- | Plus operator (@a + b@), works only for @int@ and @string@.
    PlusOp
  | -- | Minus operator (@a - b@), works only for @int@.
    MinusOp
  | -- | Multiply operator (@a * b@), works only for @int@.
    MultOp
  | -- | Divide operator (@a / b@), works only for @int@.
    DivOp
  | -- | Modulus operator (@a % b@), works only for @int@.
    ModOp
  deriving (Show)

-- | Unary operators.
data UnaryOp
  = -- | Unary plus operator (@+a@), works only for @int@.
    UnaryPlusOp
  | -- | Unary minus operator (@-a@), works only for @int@.
    UnaryMinusOp
  | -- | Not operator (@!a@), works only for @bool@.
    NotOp
  deriving (Show)

---------------------------------------------------------Types----------------------------------------------------------

-- * Types

-- | All existing types.
data Type
  = -- | 32-bit/64-bit (depending on the machine) integer type.
    TInt
  | -- | Boolean type.
    TBool
  | -- | String type.
    TString
  | -- | Array type, see 'ArrayType'.
    TArray ArrayType
  | -- | Function type, see 'FunctionType'.
    TFunction FunctionType
  deriving (Show)

-- | Array type, it contains the length of the array and its elements type.
--
-- > [3 + 4] int
data ArrayType = ArrayType {arrElementT :: Type, arrLength :: Expression}
  deriving (Show)

-- | Function type, it contains the result of the function and its parameters types.
--
-- > func (int, string) bool
--
-- > func ([3] int)
data FunctionType = FunctionType {funcParamsTs :: [Type], funcResultT :: MaybeVoid Type}
  deriving (Show)

-------------------------------------------------------Statements-------------------------------------------------------

-- * Statements

-- | Statement.
data Statement
  = -- | Return statement with optional return value.
    StmtReturn (MaybeVoid Expression)
  | -- | For goto statement, see 'ForGoTo'.
    StmtForGoTo ForGoTo
  | -- | For statement, see 'For'.
    StmtFor For
  | -- | Var declaration statement, see 'VarDecl'.
    StmtVarDecl VarDecl
  | -- | If-else statement, see 'IfElse'.
    StmtIfElse IfElse
  | -- | Block statement, see 'Block'.
    --
    -- > { 34; foo(34); if true {} else {}; return 17; }
    StmtBlock Block
  | -- | Simple statement, see 'SimpleStmt'.
    StmtSimple SimpleStmt
  deriving (Show)

-- | Block of statements.
type Block = [Statement]

-- | For goto statement (either @break@ or @continue@), should be inside @for@.
data ForGoTo
  = -- | Break statement.
    Break
  | -- | Continue statement.
    Continue
  deriving (Show)

-- | For statement, can represent any of the 3 possible @for@ kinds.
--
-- For kind, represents classic @for@ loop.
--
-- > for i := 0; i < n; i++ {
-- >   foo(i * i);
-- > }
--
-- > for ; ; {} // same as `for {}`
--
-- While kind, represents classic @while@ loop.
--
-- > for i < n {
-- >   foo(i * i);
-- >   i = i + 2;
-- > }
--
-- Loop kind, represents endless loop (@while true@).
--
-- > for {
-- >   temp := foo(i * i * i);
-- >   if temp == 108 { break; }
-- >   i = i + 23;
data For = For {forHead :: ForHead, forBody :: Block}
  deriving (Show)

-- | For statement, can represent any of the 3 possible @for@ kinds.
data ForHead = ForHead
  { forPreStmt :: Maybe SimpleStmt,
    forCondition :: Maybe Expression,
    forPostStmt :: Maybe SimpleStmt
  }
  deriving (Show)

-- | Var declaration.
--
-- > var x int = 3
--
-- > var y = "hello"
--
-- > var z int
data VarDecl = VarDecl {varName :: Identifier, varValue :: VarValue}
  deriving (Show)

-- | Var value.
data VarValue
  = VarValue (Maybe Type) Expression
  | DefaultedVarValue Type
  deriving (Show)

-- | If-else statement.
--
-- > if i < 42 { return "hello"; } else { return "goodbye"; }
--
-- > if true { println("hello"); }
data IfElse = IfElse
  { ifPreStmt :: Maybe SimpleStmt,
    ifCondition :: Expression,
    ifBody :: Block,
    elseStmt :: Else
  }
  deriving (Show)

-- | Else part of the if-else statement.
data Else = NoElse | Else Block | Elif IfElse
  deriving (Show)

-- | Simple statement, its main difference between other statements is that it can be used inside @for@ \"pre\" and \"post\" statements.
--
-- > for i := 0; i < n; i++ { println(i); }
data SimpleStmt
  = -- | Assignment statement (e.g., @x = 17@, @a[3] = \"42\"@).
    StmtAssignment Lvalue Expression
  | -- | Increment or decrement statement (e.g., @x++@, @a[3]++@, @x--@, @a[3]--@).
    StmtIncDec Lvalue IncDec
  | -- | Short var declaration statement (e.g., @x := 3@, @y := true@).
    StmtShortVarDecl Identifier Expression
  | -- | Expression statement.
    StmtExpression Expression
  deriving (Show)

-- | Lvalue, i.e. an expression that can be placed on the left-hand side of the assignment, increment, or decrement statements.
data Lvalue
  = -- | Any variable can be lvalue (e.g., @x = 3@, @x++@).
    LvalVar Identifier
  | -- | Any array element can be lvalue (e.g., @a[5][7] = 3@, @a[0]++@).
    LvalArrEl Identifier [Expression]
  deriving (Show)

-- | Increment or decrement.
data IncDec = Inc | Dec
  deriving (Show)

---------------------------------------------------------Values---------------------------------------------------------

-- * Values

-- | Literal, array, or function value.
data Value
  = -- | Int literal value (e.g., @17@, @0xFF@, @0b101001@).
    ValInt Integer
  | -- | Boolean literal value (e.g., @true@, @false@).
    ValBool Bool
  | -- | String literal value (e.g., @\"Hello\"@, @\"\"@, @\"Some\\ntext\"@).
    ValString Text
  | -- | Array value, see 'ArrayValue'.
    ValArray ArrayValue
  | -- | Function value, see 'FunctionValue'.
    ValFunction FunctionValue
  deriving (Show)

-- | Array value.
--
-- > [3] int {1, 2}
--
-- > [10] bool {}
data ArrayValue = ArrayValue {arrT :: ArrayType, arrElements :: [Expression]}
  deriving (Show)

-- | Function value.
data FunctionValue
  = -- | Anonymous function, see 'Function'.
    --
    -- > func (x int) int { return x * x; }
    --
    -- > func () {}
    AnonymousFunction Function
  | -- | Null literal (@nil@).
    Nil
  deriving (Show)

-- | Function representation without name.
--
-- It contains the result of the function, its parameters, and its body.
data Function = Function {funcParams :: [(Identifier, Type)], funcResult :: MaybeVoid Type, funcBody :: Block}
  deriving (Show)

-- | Any valid identifier (e.g., @he42llo@, @_42@).
type Identifier = Text
