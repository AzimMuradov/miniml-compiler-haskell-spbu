module Transformations.AnfPrettyPrinter (prettyPrint) where

import Data.List.NonEmpty (toList)
import Data.Text (unpack)
import Transformations.Anf

prettyPrint :: Program -> String
prettyPrint (Program stmts) = prettyPrint' stmts

prettyPrint' :: [Statement] -> String
prettyPrint' stmts = unlines $ map prettyStmt stmts

prettyStmt :: Statement -> String
prettyStmt (StmtDecl name expr) = "let " <> prettyIdentifier name <> " " <> prettyExpr expr
prettyStmt (StmtExpr expr) = prettyExpr expr

prettyExpr :: Expression -> String
prettyExpr (ExprAtom aexpr) = prettyAtomic aexpr
prettyExpr (ExprComp cexpr) = prettyComplex cexpr
prettyExpr (ExprLetIn name value expr) =
  "let "
    <> prettyIdentifier name
    <> ( case value of
           (ExprAtom aexpr@(AtomClosure _ _)) -> " " <> prettyAtomic aexpr
           _ -> " = " <> prettyExpr value
       )
    <> " in "
    <> prettyExpr expr

prettyAtomic :: AtomicExpression -> String
prettyAtomic aexpr = case aexpr of
  AtomIdentifier name -> prettyIdentifier name
  AtomUnit -> parens ""
  AtomBool value -> if value then "true" else "false"
  AtomInt value -> show value
  AtomClosure args expr -> unwords (map prettyIdentifier $ toList args) <> " = " <> prettyExpr expr

prettyComplex :: ComplexExpression -> String
prettyComplex cexpr = case cexpr of
  CompApp f args -> parens $ prettyAtomic f <> unwords (map (\x -> " " <> prettyAtomic x) $ toList args)
  CompIte c t e ->
    concat
      [ "if ",
        prettyAtomic c,
        " then ",
        prettyExpr t,
        " else ",
        prettyExpr e
      ]

prettyIdentifier :: Identifier -> String
prettyIdentifier ident = "`" <> unpack ident <> "`"

parens :: String -> String
parens val = "(" <> val <> ")"
