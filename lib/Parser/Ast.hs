{-# LANGUAGE DuplicateRecordFields #-}

module Parser.Ast where

import Data.Text (Text)

-- * Main program

newtype Program = Program [Statement]
  deriving (Show, Eq)

-------------------------------------------------------Statements-------------------------------------------------------

-- * Statements

-- | Statement.
data Statement
  = -- | Expression statement.
    StmtExpr Expression
  | -- | ( let x = 5 )
    StmtVarDecl VarDecl
  | -- | ( let f x y = x + y )
    StmtFunDecl FunDecl
  | -- | ( let rec f x y = f x 1 + f 1 y)
    StmtRecFunDecl RecFunDecl
  deriving (Show, Eq)

-- ** Declarations

-- TODO : Add docs
data VarDecl = VarDecl (Identifier, Maybe Type) [Expression] deriving (Show, Eq)

-- TODO : Add docs
data FunDecl = FunDecl Identifier Fun deriving (Show, Eq)

-- TODO : Add docs
data RecFunDecl = RecFunDecl Identifier Fun deriving (Show, Eq)

------------------------------------------------------Expressions-------------------------------------------------------

-- * Expressions

-- | Expression.
data Expression
  = -- | ( "x", "y" )
    ExprIdentifier Identifier
  | -- | ( 5, 7.4, fun x y = x + y )
    ExprValue Value
  | -- | ( +, - )
    ExprOperations Operations
  | -- | ( f 6, (fun x y = x + y) 5 6 )
    ExprApplication Expression Expression
  | -- | ( if cond then e1 else e2 )
    ExprIf Expression [Expression] [Expression]
  | -- | ( let x = 4 in ... )
    ExprLetInV (Identifier, Maybe Type) [Expression] [Expression]
  | -- | ( let f x y = x + y in ... )
    ExprLetInF Identifier Fun [Expression]
  | -- | ( let rec f x y = x + y in ... )
    ExprLetRecInF Identifier Fun [Expression]
  deriving (Show, Eq)

-- * Types

-- ** BasicTypes

-- | Type.
data Type
  = TBool
  | TInt
  | TFun Type Type
  deriving (Show, Eq)

-- * Values

-- | Value.
data Value
  = ValBool Bool
  | ValInt Integer
  | ValFun Fun
  deriving (Show, Eq)

-- | ( fun x y -> x + y )
data Fun = Fun [(Identifier, Maybe Type)] [Expression]
  deriving (Show, Eq)

-- * Operators

-- TODO : Add docs
data Operations
  = BooleanOp BooleanOp
  | -- | ( not )
    NotOp Expression
  | ArithmeticOp ArithmeticOp
  | ComparisonOp ComparisonOp
  deriving (Show, Eq)

-- TODO : Add docs
data BooleanOp
  = -- | ( && )
    AndOp {bL :: Expression, bR :: Expression}
  | -- | ( || )
    OrOp {bL :: Expression, bR :: Expression}
  deriving (Show, Eq)

-- TODO : Add docs
data ArithmeticOp
  = -- | ( + )
    PlusOp {aL :: Expression, aR :: Expression}
  | -- | ( - )
    MinusOp {aL :: Expression, aR :: Expression}
  | -- | ( * )
    MulOp {aL :: Expression, aR :: Expression}
  | -- | ( / )
    DivOp {aL :: Expression, aR :: Expression}
  deriving (Show, Eq)

-- TODO : Add docs
data ComparisonOp
  = -- | ( = )
    EqOp {cL :: Expression, cR :: Expression}
  | -- | ( <> )
    NeOp {cL :: Expression, cR :: Expression}
  | -- | ( < )
    LtOp {cL :: Expression, cR :: Expression}
  | -- | ( <= )
    LeOp {cL :: Expression, cR :: Expression}
  | -- | ( > )
    MtOp {cL :: Expression, cR :: Expression}
  | -- | ( >= )
    MeOp {cL :: Expression, cR :: Expression}
  deriving (Show, Eq)

-- * Identifier

-- | Any valid identifier.
type Identifier = Text
