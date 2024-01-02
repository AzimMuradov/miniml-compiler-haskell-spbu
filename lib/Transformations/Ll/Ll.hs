{-# LANGUAGE LambdaCase #-}

module Transformations.Ll.Ll (llAst) where

import Control.Monad.State (State, gets, modify, runState)
import qualified Data.List.NonEmpty as NE
import Data.Text (pack)
import MonadUtils
import qualified Transformations.Ll.Lfr as Lfr
import qualified Transformations.Simplifier.SimplifiedAst as Ast
import qualified Trees.Common as Common

-- * AST Lambda Lifter

llAst :: Ast.Program -> Lfr.Program
llAst (Ast.Program gDecls cnt) =
  let (gDecls', Env _ cnt') = runState (mapM llGDecl gDecls) (Env [] cnt)
   in Lfr.Program (concat gDecls') cnt'

-- * Internal

-- ** Lambda Lifter State

type LlState = State Env

data Env = Env
  { genFunDecls :: [FunDeclaration],
    idCnt :: Common.IdCnt
  }

data FunDeclaration = FunDecl Common.Identifier' [Common.Identifier'] Lfr.Expression

-- ** Lambda Lifters

llGDecl :: Ast.Declaration -> LlState [Lfr.GlobalDeclaration]
llGDecl = \case
  Ast.DeclVar ident value -> do
    varDecl <- ll1 (Lfr.VarDecl ident) value
    return [Lfr.GlobVarDecl varDecl]
  Ast.DeclFun ident _ (Ast.Fun params body) -> do
    fun <- ll1 (FunDecl ident (NE.toList params)) body
    genFuns <- gets genFunDecls
    modify $ \env -> env {genFunDecls = []}
    let convertFunDecl (FunDecl i ps b) = Lfr.GlobFunDecl i ps b
    return $ convertFunDecl <$> reverse (fun : genFuns)

llExpr :: Ast.Expression -> LlState Lfr.Expression
llExpr = \case
  Ast.ExprId ident -> return $ Lfr.ExprId ident
  Ast.ExprPrimVal val -> return $ Lfr.ExprVal val
  Ast.ExprBinOp op lhs rhs -> ll2 (Lfr.ExprBinOp op) lhs rhs
  Ast.ExprUnOp op x -> ll1 (Lfr.ExprUnOp op) x
  Ast.ExprApp f arg -> ll2 Lfr.ExprApp f arg
  Ast.ExprIte c t e -> ll3 Lfr.ExprIte c t e
  Ast.ExprLetIn decl expr -> case decl of
    Ast.DeclVar ident value -> do
      varDecl <- ll1 (Lfr.VarDecl ident) value
      expr' <- llExpr expr
      return $ Lfr.ExprLetIn varDecl expr'
    Ast.DeclFun ident _ fun -> llFun ident fun >> llExpr expr
  Ast.ExprFun fun -> do
    ident <- genId
    llFun ident fun
    return $ Lfr.ExprId ident

llFun :: Common.Identifier' -> Ast.Fun -> LlState ()
llFun ident (Ast.Fun params body) = do
  fun <- ll1 (FunDecl ident (NE.toList params)) body
  modify $ \env -> env {genFunDecls = fun : genFunDecls env}

-- ** Identifier Generation

genId :: LlState Common.Identifier'
genId = do
  cnt <- gets idCnt
  modify $ \env -> env {idCnt = cnt + 1}
  return $ Common.Gen cnt $ pack "ll"

-- ** Utils

ll1 ::
  (Lfr.Expression -> a) ->
  (Ast.Expression -> LlState a)
ll1 = liftM1' llExpr

ll2 ::
  (Lfr.Expression -> Lfr.Expression -> a) ->
  (Ast.Expression -> Ast.Expression -> LlState a)
ll2 = liftM2' llExpr

ll3 ::
  (Lfr.Expression -> Lfr.Expression -> Lfr.Expression -> a) ->
  (Ast.Expression -> Ast.Expression -> Ast.Expression -> LlState a)
ll3 = liftM3' llExpr
