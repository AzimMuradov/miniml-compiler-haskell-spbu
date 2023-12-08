module Transformations.Anf.PrettyPrinter (prettyPrint) where

import Data.Text (unpack)
import StdLib (binOpIdentifier, unOpIdentifier)
import Transformations.Anf.Anf
import Trees.Common (Identifier' (Gen, Txt))

prettyPrint :: Program -> String
prettyPrint (Program stmts) = prettyPrint' stmts

prettyPrint' :: [GlobalDeclaration] -> String
prettyPrint' stmts = unlines $ map prettyStmt stmts

prettyStmt :: GlobalDeclaration -> String
prettyStmt (GlobVarDecl name value) = doubleSemicolon $ unwords ["let", prettyId name, "=", prettyExpr value]
prettyStmt (GlobFunDecl name params body) = doubleSemicolon $ unwords ["let", prettyId name, unwords (prettyId <$> params), "=", prettyExpr body]

prettyExpr :: Expression -> String
prettyExpr (ExprAtom aexpr) = prettyAtomic aexpr
prettyExpr (ExprComp cexpr) = prettyComplex cexpr
prettyExpr (ExprLetIn (ident, val) expr) = unwords ["let", prettyId ident, "=", prettyExpr val, "in", prettyExpr expr]

prettyAtomic :: AtomicExpression -> String
prettyAtomic aexpr = case aexpr of
  AtomId name -> prettyId name
  AtomBool value -> if value then "true" else "false"
  AtomInt value -> show value
  AtomBinOp op lhs rhs -> parens (unwords [prettyAtomic lhs, unpack $ binOpIdentifier op, prettyAtomic rhs])
  AtomUnOp op x -> parens $ unpack (unOpIdentifier op) <> prettyAtomic x

prettyComplex :: ComplexExpression -> String
prettyComplex cexpr = case cexpr of
  CompApp f arg -> parens $ prettyAtomic f <> " " <> prettyAtomic arg
  CompIte c t e -> unwords ["if", prettyAtomic c, "then", prettyAtomic t, "else", prettyAtomic e]

prettyId :: Identifier' -> String
prettyId (Txt n) = unpack n
prettyId (Gen n ident) = unpack ident <> "'" <> show n

parens :: String -> String
parens val = "(" <> val <> ")"

doubleSemicolon :: String -> String
doubleSemicolon val = val
