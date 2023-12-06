module Transformations.Anf.PrettyPrinter (prettyPrint) where

import Data.Text (unpack)
import Transformations.Anf.Anf
import Trees.Common (Identifier' (Gen, Txt))

prettyPrint :: Program -> String
prettyPrint (Program stmts) = prettyPrint' stmts

prettyPrint' :: [GlobalDeclaration] -> String
prettyPrint' stmts = unlines $ map prettyStmt stmts

prettyStmt :: GlobalDeclaration -> String
prettyStmt (GlobVarDecl name value) = doubleSemicolon $ unwords ["let", prettyIdentifier name, "=", prettyExpr value]
prettyStmt (GlobFunDecl name params body) = doubleSemicolon $ unwords ["let", prettyIdentifier name, undefined, "=", prettyExpr body]

prettyExpr :: Expression -> String
prettyExpr (ExprAtom aexpr) = prettyAtomic aexpr
prettyExpr (ExprComp cexpr) = prettyComplex cexpr
prettyExpr (ExprLetIn decls expr) = unwords ["let", prettyIdentifier undefined, "=", prettyExpr undefined, "in", prettyExpr expr]

prettyAtomic :: AtomicExpression -> String
prettyAtomic aexpr = case aexpr of
  AtomId name -> prettyIdentifier name
  AtomBool value -> if value then "true" else "false"
  AtomInt value -> show value
  AtomBinOp op lhs rhs -> undefined
  AtomUnOp op x -> undefined

prettyComplex :: ComplexExpression -> String
prettyComplex cexpr = case cexpr of
  CompApp f arg -> parens $ prettyAtomic f <> " " <> prettyAtomic arg
  CompIte c t e -> unwords ["if", prettyAtomic c, "then", prettyAtomic t, "else", prettyAtomic e]

prettyIdentifier :: Identifier' -> String
prettyIdentifier (Txt n) = "`" <> unpack n <> "`"
prettyIdentifier (Gen n) = "`$" <> show n <> "`"

parens :: String -> String
parens val = "(" <> val <> ")"

doubleSemicolon :: String -> String
doubleSemicolon val = val <> ";;"
