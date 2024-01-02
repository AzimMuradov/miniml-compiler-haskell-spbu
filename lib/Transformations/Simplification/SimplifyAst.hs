{-# LANGUAGE LambdaCase #-}

module Transformations.Simplification.SimplifyAst (simplifyAst) where

import Control.Monad.State (State, get, modify, runState)
import Data.Text (pack)
import MonadUtils
import qualified Parser.Ast as Ast
import qualified Transformations.Simplification.SimplifiedAst as SAst
import qualified Trees.Common as Common

-- * AST Simplifier

simplifyAst :: Ast.Program -> SAst.Program
simplifyAst (Ast.Program stmts) = uncurry SAst.Program $ runState (mapM simplifyStmt stmts) 0

-- * Internal

-- ** Simplifier State

type SimplifierState = State Common.IdCnt

-- ** Simplifiers

simplifyStmt :: Ast.Statement -> SimplifierState SAst.Declaration
simplifyStmt = \case
  Ast.StmtDecl decl -> simplifyDecl decl
  Ast.StmtExpr expr -> SAst.DeclVar <$> genId <*> simplifyExpr expr

simplifyExpr :: Ast.Expression -> SimplifierState SAst.Expression
simplifyExpr = \case
  Ast.ExprId ident -> return $ SAst.ExprId (convertId ident)
  Ast.ExprVal val -> return $ SAst.ExprVal val
  Ast.ExprBinOp op lhs rhs -> simplify2 (SAst.ExprBinOp op) lhs rhs
  Ast.ExprUnOp op x -> simplify1 (SAst.ExprUnOp op) x
  Ast.ExprApp f arg -> simplify2 SAst.ExprApp f arg
  Ast.ExprIte c t e -> simplify3 SAst.ExprIte c t e
  Ast.ExprLetIn decl expr -> SAst.ExprLetIn <$> simplifyDecl decl <*> simplifyExpr expr
  Ast.ExprFun fun -> SAst.ExprFun <$> simplifyFun fun

simplifyDecl :: Ast.Declaration -> SimplifierState SAst.Declaration
simplifyDecl = \case
  Ast.DeclVar ident value -> simplify1 (SAst.DeclVar (convertTypedId ident)) value
  Ast.DeclFun ident isRec fun -> SAst.DeclFun (convertId ident) isRec <$> simplifyFun fun

simplifyFun :: Ast.Fun -> SimplifierState SAst.Fun
simplifyFun (Ast.Fun params _ body) = simplify1 (SAst.Fun (convertTypedId <$> params)) body

-- ** Identifier Conversion and Generation

convertId :: Common.Identifier -> Common.Identifier'
convertId = Common.Txt

convertTypedId :: (Common.Identifier, Maybe Common.Type) -> Common.Identifier'
convertTypedId = Common.Txt . fst

genId :: SimplifierState Common.Identifier'
genId = do
  cnt <- get
  modify (+ 1)
  return $ Common.Gen cnt $ pack "simp"

-- ** Utils

simplify1 ::
  (SAst.Expression -> a) ->
  (Ast.Expression -> SimplifierState a)
simplify1 = liftM1' simplifyExpr

simplify2 ::
  (SAst.Expression -> SAst.Expression -> a) ->
  (Ast.Expression -> Ast.Expression -> SimplifierState a)
simplify2 = liftM2' simplifyExpr

simplify3 ::
  (SAst.Expression -> SAst.Expression -> SAst.Expression -> a) ->
  (Ast.Expression -> Ast.Expression -> Ast.Expression -> SimplifierState a)
simplify3 = liftM3' simplifyExpr
