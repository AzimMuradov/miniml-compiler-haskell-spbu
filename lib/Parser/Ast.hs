{-# LANGUAGE DuplicateRecordFields #-}

module Parser.Ast where

import Data.Text (Text)

-- * Main program

newtype Program = Program [Statement]
  deriving (Show)

-------------------------------------------------------Statements-------------------------------------------------------

-- * Statements

-- | Statement.
data Statement
  = -- | Expression statement.
    SExpr Expr
  | -- | ( let x = 5 )
    SVarDecl VarDecl
  | -- | ( let f x y = x + y )
    SFunDecl FunDecl
  | -- | ( let rec f x y = f x 1 + f 1 y)
    SRecFunDecl RecFunDecl
  deriving (Show, Eq)

-- ** Declarations

-- TODO : Add docs
data VarDecl = VarDecl (Identifier, Maybe Type) [Expr] deriving (Show, Eq)

-- TODO : Add docs
data FunDecl = FunDecl Identifier Fun deriving (Show, Eq)

-- TODO : Add docs
data RecFunDecl = RecFunDecl Identifier Fun deriving (Show, Eq)

------------------------------------------------------Expressions-------------------------------------------------------

-- * Expressions

-- | Expression.
data Expr
  = -- | ( "x", "y" )
    EIdentifier Identifier
  | -- | ( 5, 7.4, fun x y = x + y )
    EValue Value
  | -- | ( +, - )
    EOperations Operations
  | -- | ( f 6, (fun x y = x + y) 5 6 )
    EApplication Expr Expr
  | -- | ( if cond then e1 else e2 )
    EIf Expr [Expr] [Expr]
  | -- | ( let x = 4 in ... )
    ELetInV (Identifier, Maybe Type) [Expr] [Expr]
  | -- | ( let f x y = x + y in ... )
    ELetInF Identifier Fun [Expr]
  | -- | ( let rec f x y = x + y in ... )
    ELetRecInF Identifier Fun [Expr]
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
  = VBool Bool
  | VInt Integer
  | VFun Fun
  deriving (Show, Eq)

-- | ( fun x y -> x + y )
data Fun = Fun [(Identifier, Maybe Type)] [Expr]
  deriving (Show, Eq)

-- * Operators

-- TODO : Add docs
data Operations
  = BooleanOp BooleanOp
  | -- | ( not )
    NotOp Expr
  | ArithmeticOp ArithmeticOp
  | ComparisonOp ComparisonOp
  deriving (Show, Eq)

-- TODO : Add docs
data BooleanOp
  = -- | ( && )
    AndOp {bL :: Expr, bR :: Expr}
  | -- | ( || )
    OrOp {bL :: Expr, bR :: Expr}
  deriving (Show, Eq)

-- TODO : Add docs
data ArithmeticOp
  = -- | ( + )
    PlusOp {aL :: Expr, aR :: Expr}
  | -- | ( - )
    MinusOp {aL :: Expr, aR :: Expr}
  | -- | ( * )
    MulOp {aL :: Expr, aR :: Expr}
  | -- | ( / )
    DivOp {aL :: Expr, aR :: Expr}
  deriving (Show, Eq)

-- TODO : Add docs
data ComparisonOp
  = -- | ( = )
    EqOp {cL :: Expr, cR :: Expr}
  | -- | ( <> )
    NeOp {cL :: Expr, cR :: Expr}
  | -- | ( < )
    LtOp {cL :: Expr, cR :: Expr}
  | -- | ( <= )
    LeOp {cL :: Expr, cR :: Expr}
  | -- | ( > )
    MtOp {cL :: Expr, cR :: Expr}
  | -- | ( >= )
    MeOp {cL :: Expr, cR :: Expr}
  deriving (Show, Eq)

-- * Identifier

-- | Any valid identifier.
type Identifier = Text
