{-# LANGUAGE DuplicateRecordFields #-}

module Parser.Ast where

import Data.Text (Text)

-- * Main program

newtype Program = Program [Statement]
  deriving (Show)

-- * Declarations

data VarDecl = VarDecl (Identifier, Maybe Type) [Expr] deriving (Show, Eq)

data FunDecl = FunDecl Identifier Fun deriving (Show, Eq)

data RecFunDecl = RecFunDecl Identifier Fun deriving (Show, Eq)

-- * Statements

data Statement
  = SExpr Expr
  | SVarDecl VarDecl
  | -- \| ( let x = 5 )
    SFunDecl FunDecl
  | -- \| ( let f x y = x + y )
    SRecFunDecl RecFunDecl
  -- ( let rec f x y = f x 1 + f 1 y)
  deriving (Show, Eq)

-- * Expressions

data Expr
  = EIdentifier Identifier
  | -- \| ( "x", "y" )
    EValue Value
  | -- \| ( 5, 7.4, fun x y = x + y )
    EOperations Operations
  | -- \| ( +, - )
    EApplication Expr Expr
  | -- \| ( f 6, (fun x y = x + y) 5 6 )
    EIf Expr [Expr] [Expr]
  | -- \| ( if cond then e1 else e2 )
    ELetInV (Identifier, Maybe Type) [Expr] [Expr]
  | -- \| ( let x = 4 in ... )
    ELetInF Identifier Fun [Expr]
  -- \| ( let f x y = x + y in ... )
  deriving (Show, Eq)

-- * Types

-- ** BasicTypes

data Type
  = TBool
  | TInt
  | TFun Type Type
  deriving (Show, Eq)

-- * Values

data Value
  = VBool Bool
  | VInt Integer
  | VFun Fun
  deriving (Show, Eq)

data Fun = Fun [(Identifier, Maybe Type)] [Expr] deriving (Show, Eq)

-- | ( fun x y -> x + y )

-- * Operators

data Operations
  = BooleanOp BooleanOp
  | NotOp Expr
  | -- \| ( not )
    ArithmeticOp ArithmeticOp
  | ComparisonOp ComparisonOp
  deriving (Show, Eq)

data BooleanOp
  = AndOp {bL :: Expr, bR :: Expr}
  | -- \| ( && )
    OrOp {bL :: Expr, bR :: Expr}
  -- \| ( || )
  deriving (Show, Eq)

data ArithmeticOp
  = PlusOp {aL :: Expr, aR :: Expr}
  | -- \| ( + )
    MinusOp {aL :: Expr, aR :: Expr}
  | -- \| ( - )
    MulOp {aL :: Expr, aR :: Expr}
  | -- \| ( * )
    DivOp {aL :: Expr, aR :: Expr}
  -- \| ( / )
  deriving (Show, Eq)

data ComparisonOp
  = EqOp {cL :: Expr, cR :: Expr}
  | -- \| ( = )
    NeOp {cL :: Expr, cR :: Expr}
  | -- \| ( <> )
    LtOp {cL :: Expr, cR :: Expr}
  | -- \| ( < )
    LeOp {cL :: Expr, cR :: Expr}
  | -- \| ( <= )
    MtOp {cL :: Expr, cR :: Expr}
  | -- \| ( > )
    MeOp {cL :: Expr, cR :: Expr}
  -- \| ( >= )
  deriving (Show, Eq)

-- * Identifier

type Identifier = Text
