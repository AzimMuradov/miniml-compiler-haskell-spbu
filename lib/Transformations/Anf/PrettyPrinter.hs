{-# LANGUAGE LambdaCase #-}

module Transformations.Anf.PrettyPrinter (prettyPrint) where

import Control.Monad.State (MonadState (get), State, evalState, modify)
import Data.Text (unpack)
import Transformations.Anf.Anf
import Trees.Common

prettyPrint :: Program -> String
prettyPrint (Program decls) = unlines $ prettyDecl <$> decls

type IndentState = State IndentLevel

type IndentLevel = Int

prettyDecl :: GlobalDeclaration -> String
prettyDecl decl = evalState (prettyDecl' decl) 0 <> ";;"
  where
    prettyDecl' :: GlobalDeclaration -> IndentState String
    prettyDecl' (GlobVarDecl name value) = do
      val' <- prettyExpr value
      return $ unwords ["let", prettyId name, "=", val']
    prettyDecl' (GlobFunDecl name params body) = do
      val' <- prettyExpr body
      return $ unwords ["let", prettyId name, unwords (prettyId <$> params), "=", val']

prettyExpr :: Expression -> IndentState String
prettyExpr (ExprAtom aexpr) = return $ prettyAtomic aexpr
prettyExpr (ExprComp cexpr) = prettyComplex cexpr
prettyExpr (ExprLetIn (ident, val) expr) = do
  modify (+ 2)
  indent <- get
  val' <- prettyExpr val
  expr' <- prettyExpr expr
  let declText = createIndent indent <> "let " <> prettyId ident <> " = " <> val'
  let exprText = createIndent indent <> "in " <> expr'
  modify $ \x -> x - 2
  return $ declText <> exprText

prettyComplex :: ComplexExpression -> IndentState String
prettyComplex = \case
  CompApp f arg -> return $ parens $ prettyId f <> " " <> prettyAtomic arg
  CompIte c t e -> do
    modify (+ 4)
    indent <- get
    let cText = createIndent (indent - 2) <> "if " <> prettyAtomic c
    t' <- prettyExpr t
    let tText = createIndent indent <> "then " <> t'
    e' <- prettyExpr e
    let eText = createIndent indent <> "else " <> e'
    modify $ \x -> x - 4
    return $ cText <> tText <> eText

prettyAtomic :: AtomicExpression -> String
prettyAtomic = \case
  AtomId name -> prettyId name
  AtomUnit -> "()"
  AtomBool value -> if value then "true" else "false"
  AtomInt value -> show value
  AtomBinOp op lhs rhs -> parens (unwords [prettyAtomic lhs, prettyBinOp op, prettyAtomic rhs])
  AtomUnOp op x -> parens $ prettyUnOp op <> prettyAtomic x

prettyId :: Identifier' -> String
prettyId (Txt n) = unpack n
prettyId (Gen n ident) = unpack ident <> "'" <> show n

prettyBinOp :: BinaryOperator -> String
prettyBinOp = \case
  BoolOp AndOp -> "&&"
  BoolOp OrOp -> "||"
  ArithOp PlusOp -> "+"
  ArithOp MinusOp -> "-"
  ArithOp MulOp -> "*"
  ArithOp DivOp -> "/"
  CompOp EqOp -> "="
  CompOp NeOp -> "<>"
  CompOp LtOp -> "<"
  CompOp LeOp -> "<="
  CompOp GtOp -> ">"
  CompOp GeOp -> ">="

prettyUnOp :: UnaryOperator -> String
prettyUnOp UnMinusOp = "-"

createIndent :: Int -> String
createIndent indent = "\n" <> replicate indent ' '

parens :: String -> String
parens val = "(" <> val <> ")"
