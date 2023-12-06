{-# LANGUAGE LambdaCase #-}

module Transformations.Ll.Ll (llAst) where

import Control.Monad.State (State, get, modify, runState)
import qualified Data.List.NonEmpty as NE
import qualified Transformations.Ll.Lfr as Lfr
import qualified Transformations.Simplification.SimplifiedAst as Ast
import qualified Trees.Common as Common
import Utils (liftM2', liftM3')

llAst :: Ast.Program -> Lfr.Program
llAst (Ast.Program tlDecls cnt) =
  let (tlDecls', Env _ cnt') = runState (mapM llDecl tlDecls) (Env [] cnt)
   in Lfr.Program (concat tlDecls') cnt'

-- Implementation

type LlState = State Env

data Env = Env {genFuns :: [FunDeclaration], idCnt :: Common.IdCnt}

data FunDeclaration = FunDecl Common.Identifier' [Common.Identifier'] Lfr.Expression

llDecl :: Ast.Declaration -> LlState [Lfr.TopLevelDeclaration]
llDecl = \case
  Ast.DeclVar ident ex -> sequence [ll1 (Lfr.TopLevelVarDecl . Lfr.VarDecl ident) ex]
  Ast.DeclFun ident _ (Ast.Fun params body) -> do
    f <- ll1 (FunDecl ident (NE.toList params)) body
    Env genDecl _ <- get
    modify $ \env -> env {genFuns = []}
    return $ (\(FunDecl i ps b) -> Lfr.TopLevelFunDecl i ps b) <$> reverse (f : genDecl)

llExpr :: Ast.Expression -> LlState Lfr.Expression
llExpr = \case
  Ast.ExprId ident -> return $ Lfr.ExprId ident
  Ast.ExprVal val -> return $ Lfr.ExprVal val
  Ast.ExprBinOp op lhs rhs -> ll2 (Lfr.ExprBinOp op) lhs rhs
  Ast.ExprUnOp op x -> ll1 (Lfr.ExprUnOp op) x
  Ast.ExprApp f arg -> ll2 Lfr.ExprApp f arg
  Ast.ExprIte c t e -> ll3 Lfr.ExprIte c t e
  Ast.ExprLetIn decl expr -> case decl of
    Ast.DeclVar ident value -> do
      varDecl <- Lfr.VarDecl ident <$> llExpr value
      expr' <- llExpr expr
      return $ Lfr.ExprLetIn varDecl expr'
    Ast.DeclFun ident _ fun -> llFun ident fun >> llExpr expr
  Ast.ExprFun fun -> do
    ident <- genName
    llFun ident fun
    return $ Lfr.ExprId ident

llFun :: Common.Identifier' -> Ast.Fun -> LlState ()
llFun ident (Ast.Fun params body) = do
  funDecl <- FunDecl ident (NE.toList params) <$> llExpr body
  modify $ \env@(Env tlds _) -> env {genFuns = funDecl : tlds}

-- Name generation

genName :: LlState Common.Identifier'
genName = do
  Env _ cnt <- get
  modify $ \env -> env {idCnt = cnt + 1}
  return $ Common.Gen cnt

-- Utils

ll1 :: (Lfr.Expression -> a) -> Ast.Expression -> LlState a
ll1 f x = f <$> llExpr x

ll2 ::
  (Lfr.Expression -> Lfr.Expression -> a) ->
  Ast.Expression ->
  Ast.Expression ->
  LlState a
ll2 = liftM2' llExpr

ll3 ::
  (Lfr.Expression -> Lfr.Expression -> Lfr.Expression -> a) ->
  Ast.Expression ->
  Ast.Expression ->
  Ast.Expression ->
  LlState a
ll3 = liftM3' llExpr
