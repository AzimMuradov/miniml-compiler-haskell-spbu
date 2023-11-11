module Transformations.AstToTypelessAst (astToTypelessAst) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Tuple.Extra (uncurry3)
import qualified Parser.Ast as Ast
import StdLib (binOpIdentifier, unOpIdentifier)
import qualified Transformations.TypelessAst as TAst

astToTypelessAst :: Ast.Program -> TAst.Program
astToTypelessAst (Ast.Program stmts) = TAst.Program $ transformStmt <$> stmts

transformStmt :: Ast.Statement -> TAst.Statement
transformStmt (Ast.StmtUserDecl decl) = uncurry3 TAst.StmtDecl $ transformUserDecl decl
transformStmt (Ast.StmtExpr expr) = TAst.StmtExpr $ transformExpr expr

transformUserDecl :: Ast.UserDeclaration -> (TAst.Identifier, TAst.Expression, TAst.IsRec)
transformUserDecl (Ast.DeclVar (name, _) value) = (name, transformExpr value, False)
transformUserDecl (Ast.DeclFun name fun) = (name, TAst.ExprValue $ transformFun fun, False)
transformUserDecl (Ast.DeclRecFun name fun) = (name, TAst.ExprValue $ transformFun fun, True)

transformExpr :: Ast.Expression -> TAst.Expression
transformExpr (Ast.ExprIdentifier name) = TAst.ExprIdentifier name
transformExpr (Ast.ExprValue value) = TAst.ExprValue $ transformValue value
transformExpr (Ast.ExprBinaryOperation op lhs rhs) =
  TAst.ExprApplication
    (TAst.ExprIdentifier $ binOpIdentifier op)
    (transformExpr lhs :| [transformExpr rhs])
transformExpr (Ast.ExprUnaryOperation op x) =
  TAst.ExprApplication
    (TAst.ExprIdentifier $ unOpIdentifier op)
    (transformExpr x :| [])
transformExpr (Ast.ExprApplication f arg) =
  TAst.ExprApplication
    (transformExpr f)
    (transformExpr arg :| [])
transformExpr (Ast.ExprIte c t e) =
  let c' = transformExpr c
      t' = transformExpr t
      e' = transformExpr e
   in TAst.ExprIte c' t' e'
transformExpr (Ast.ExprLetIn decl expr) = TAst.ExprLetIn (transformUserDecl decl) (transformExpr expr)

transformValue :: Ast.Value -> TAst.Value
transformValue Ast.ValUnit = TAst.ValUnit
transformValue (Ast.ValBool bool) = TAst.ValBool bool
transformValue (Ast.ValInt int) = TAst.ValInt int
transformValue (Ast.ValFun fun) = transformFun fun

transformFun :: Ast.Fun -> TAst.Value
transformFun (Ast.Fun params _ value) = TAst.ValFun (fst <$> params) (transformExpr value)
