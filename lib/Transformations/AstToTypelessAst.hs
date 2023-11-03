module Transformations.AstToTypelessAst (astToTypelessAst) where

import Data.Maybe (mapMaybe)
import qualified Parser.Ast as Ast
import StdLib (binOpIdentifier, unOpIdentifier)
import qualified Transformations.TypelessAst as TAst

astToTypelessAst :: Ast.Program -> TAst.Program
astToTypelessAst (Ast.Program stmts) = TAst.Program $ mapMaybe transformStmt stmts

transformStmt :: Ast.Statement -> Maybe TAst.Statement
transformStmt (Ast.StmtUserDecl decl) = Just $ uncurry TAst.StmtDecl $ transformUserDecl decl
transformStmt (Ast.StmtStdDecl _) = Nothing
transformStmt (Ast.StmtExpr expr) = Just $ TAst.StmtExpr $ transformExpr expr

transformUserDecl :: Ast.UserDeclaration -> (TAst.Identifier, TAst.Expression)
transformUserDecl (Ast.DeclVar (name, _) value) = (name, transformExpr value)
transformUserDecl (Ast.DeclFun name fun) = (name, TAst.ExprValue $ transformFun fun)
transformUserDecl (Ast.DeclRecFun name fun) = (name, TAst.ExprValue $ transformFun fun)

transformExpr :: Ast.Expression -> TAst.Expression
transformExpr (Ast.ExprIdentifier name) = TAst.ExprIdentifier name
transformExpr (Ast.ExprValue value) = TAst.ExprValue $ transformValue value
transformExpr (Ast.ExprBinaryOperation op lhs rhs) =
  let app1 = TAst.ExprApplication (TAst.ExprIdentifier $ binOpIdentifier op) (transformExpr lhs)
   in TAst.ExprApplication app1 (transformExpr rhs)
transformExpr (Ast.ExprUnaryOperation op x) = TAst.ExprApplication (TAst.ExprIdentifier $ unOpIdentifier op) (transformExpr x)
transformExpr (Ast.ExprApplication f arg) = TAst.ExprApplication (transformExpr f) (transformExpr arg)
transformExpr (Ast.ExprIte c t e) = TAst.ExprIte (transformExpr c) (transformExpr t) (transformExpr e)
transformExpr (Ast.ExprLetIn decl expr) = uncurry TAst.ExprLetIn (transformUserDecl decl) (transformExpr expr)

transformValue :: Ast.Value -> TAst.Value
transformValue Ast.ValUnit = TAst.ValUnit
transformValue (Ast.ValBool bool) = TAst.ValBool bool
transformValue (Ast.ValInt int) = TAst.ValInt int
transformValue (Ast.ValFun fun) = transformFun fun

transformFun :: Ast.Fun -> TAst.Value
transformFun (Ast.Fun params _ value) = TAst.ValFun (fst <$> params) (transformExpr value)
