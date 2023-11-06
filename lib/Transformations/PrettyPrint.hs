module Transformations.PrettyPrint where

import Data.List.NonEmpty (toList)
import qualified Transformations.Anf as Anf
import Prelude hiding (lookup)
import Transformations.Anf (Identifier)
import Data.Text (unpack)

prettyPrint :: Anf.Program -> String
prettyPrint (Anf.Program stmts) = prettyPrint' stmts

prettyPrint' :: [Anf.Statement] -> String
prettyPrint' stmts = unlines $ map prettyStmt stmts

prettyStmt :: Anf.Statement -> String
prettyStmt (Anf.StmtDecl name expr) =
  "let " ++ prettyIdentifier name ++ case expr of
    (Anf.ExprAtom (Anf.AtomClosure _ _)) -> " " ++ prettyExpr expr
    _ -> " = " ++ prettyExpr expr
prettyStmt (Anf.StmtExpr expr) = prettyExpr expr

prettyExpr :: Anf.Expression -> String
prettyExpr (Anf.ExprAtom aexpr) = prettyAtomic aexpr
prettyExpr (Anf.ExprComp cexpr) = prettyComplex cexpr
prettyExpr (Anf.ExprLetIn name value expr) = "let " ++ prettyIdentifier name ++ " = " ++ prettyExpr value ++ " in " ++ prettyExpr expr

prettyAtomic :: Anf.AtomicExpression -> String
prettyAtomic aexpr = case aexpr of
  Anf.AtomIdentifier name -> prettyIdentifier name
  Anf.AtomUnit -> parens ""
  Anf.AtomBool value -> if value then "true" else "false"
  Anf.AtomInt value -> show value
  Anf.AtomClosure args expr -> unwords (map prettyIdentifier $ toList args) ++ " = " ++ prettyExpr expr

prettyComplex :: Anf.ComplexExpression -> String
prettyComplex cexpr = case cexpr of
  Anf.CompApp f args -> parens $ prettyAtomic f ++ unwords (map (\x -> " " ++ prettyAtomic x) $ toList args)
  Anf.CompIte c t e -> "if " ++ prettyAtomic c ++ " then " ++ prettyExpr t ++ " else " ++ prettyExpr e

prettyIdentifier :: Identifier -> String
prettyIdentifier ident = "`" ++ unpack ident ++ "`"

parens :: String -> String
parens val = "(" ++ val ++ ")"

