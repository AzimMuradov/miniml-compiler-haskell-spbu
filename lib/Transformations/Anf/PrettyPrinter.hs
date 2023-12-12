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
prettyComplex cexpr = case cexpr of
  CompApp f arg -> return $ parens $ prettyAtomic f <> " " <> prettyAtomic arg
  CompIte c t e -> do
    indent <- get
    let cText = createIndent (indent + 2) <> "if " <> prettyAtomic c
    let tText = createIndent (indent + 4) <> "then " <> prettyAtomic t
    let eText = createIndent (indent + 4) <> "else " <> prettyAtomic e
    return $ cText <> tText <> eText

prettyAtomic :: AtomicExpression -> String
prettyAtomic = \case
  AtomId name -> prettyId name
  AtomBool value -> if value then "true" else "false"
  AtomInt value -> show value
  AtomBinOp op lhs rhs -> parens (unwords [prettyAtomic lhs, prettyBinOp op, prettyAtomic rhs])
  AtomUnOp op x -> parens $ prettyUnOp op <> prettyAtomic x

prettyId :: Identifier' -> String
prettyId (Txt n) = unpack n
prettyId (Gen n ident) = unpack ident <> "'" <> show n

prettyBinOp :: BinaryOperator -> String
prettyBinOp = \case
  BooleanOp AndOp -> "&&"
  BooleanOp OrOp -> "||"
  ArithmeticOp PlusOp -> "+"
  ArithmeticOp MinusOp -> "-"
  ArithmeticOp MulOp -> "*"
  ArithmeticOp DivOp -> "/"
  ComparisonOp EqOp -> "="
  ComparisonOp NeOp -> "<>"
  ComparisonOp LtOp -> "<"
  ComparisonOp LeOp -> "<="
  ComparisonOp GtOp -> ">"
  ComparisonOp GeOp -> ">="

prettyUnOp :: UnaryOperator -> String
prettyUnOp UnaryMinusOp = "-"

createIndent :: Int -> String
createIndent indent = "\n" <> replicate indent ' '

parens :: String -> String
parens val = "(" <> val <> ")"
