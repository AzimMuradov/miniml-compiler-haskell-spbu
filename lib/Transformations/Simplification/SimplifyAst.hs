{-# LANGUAGE LambdaCase #-}

module Transformations.Simplification.SimplifyAst (simplifyAst) where

import Control.Monad.State (State, get, modify, runState)
import qualified Parser.Ast as Ast
import qualified Transformations.Simplification.SimplifiedAst as SAst
import qualified Trees.Common as Common
import Utils

simplifyAst :: Ast.Program -> SAst.Program
simplifyAst (Ast.Program stmts) = uncurry SAst.Program $ runState (mapM simplifyStmt stmts) 0

-- Implementation

type SimplifierState = State IdCnt

type IdCnt = Int

simplifyStmt :: Ast.Statement -> SimplifierState SAst.Declaration
simplifyStmt = \case
  Ast.StmtDecl decl -> simplifyDecl decl
  Ast.StmtExpr expr -> SAst.DeclVar <$> genName <*> simplifyExpr expr

simplifyExpr :: Ast.Expression -> SimplifierState SAst.Expression
simplifyExpr = \case
  Ast.ExprId ident -> return $ SAst.ExprId (Common.Txt ident)
  Ast.ExprVal val -> return $ SAst.ExprVal val
  Ast.ExprBinOp op lhs rhs -> simplify2 (SAst.ExprBinOp op) lhs rhs
  Ast.ExprUnOp op x -> simplify1 (SAst.ExprUnOp op) x
  Ast.ExprApp f arg -> simplify2 SAst.ExprApp f arg
  Ast.ExprIte c t e -> simplify3 SAst.ExprIte c t e
  Ast.ExprLetIn decl expr -> do
    decl' <- simplifyDecl decl
    expr' <- simplifyExpr expr
    return $ SAst.ExprLetIn decl' expr'
  Ast.ExprFun fun -> SAst.ExprFun <$> simplifyFun fun

simplifyDecl :: Ast.Declaration -> SimplifierState SAst.Declaration
simplifyDecl = \case
  Ast.DeclVar name value ->
    simplify1 (SAst.DeclVar (Common.Txt . fst $ name)) value
  Ast.DeclFun name isRec fun ->
    SAst.DeclFun (Common.Txt name) isRec <$> simplifyFun fun

simplifyFun :: Ast.Fun -> SimplifierState SAst.Fun
simplifyFun (Ast.Fun params _ body) = simplify1 (SAst.Fun (Common.Txt . fst <$> params)) body

-- Name generation

genName :: SimplifierState Common.Identifier'
genName = do
  cnt <- get
  modify (+ 1)
  return $ Common.Gen cnt

-- Utils

simplify1 :: (SAst.Expression -> a) -> Ast.Expression -> SimplifierState a
simplify1 f x = f <$> simplifyExpr x

simplify2 ::
  (SAst.Expression -> SAst.Expression -> a) ->
  Ast.Expression ->
  Ast.Expression ->
  SimplifierState a
simplify2 = liftM2' simplifyExpr

simplify3 ::
  (SAst.Expression -> SAst.Expression -> SAst.Expression -> a) ->
  Ast.Expression ->
  Ast.Expression ->
  Ast.Expression ->
  SimplifierState a
simplify3 = liftM3' simplifyExpr
