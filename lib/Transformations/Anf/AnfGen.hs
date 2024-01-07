{-# LANGUAGE FlexibleContexts #-}

module Transformations.Anf.AnfGen (genAnf) where

import Control.Monad.Cont (ContT, mapContT)
import Control.Monad.State (MonadState, MonadTrans (lift), State, evalState, get, modify)
import Control.Monad.Trans.Cont (evalContT)
import Data.Text (pack)
import qualified Transformations.Anf.Anf as Anf
import qualified Transformations.Ll.Lfr as Lfr
import qualified Trees.Common as Common

-- * ANF Generator

genAnf :: Lfr.Program -> Anf.Program
genAnf (Lfr.Program gDecls cnt) =
  let gDecls' = evalState (mapM genDecl gDecls) cnt
   in Anf.Program gDecls'

-- * Internal

-- ** ANF Normalizer Continuation & Generator State

type NormCont r a = ContT r AnfGenState a

type AnfGenState = State Env

type Env = Common.IdCnt

-- ** ANF Generators

genDecl :: Lfr.GlobalDeclaration -> AnfGenState Anf.GlobalDeclaration
genDecl (Lfr.GlobVarDecl (Lfr.VarDecl ident value)) = Anf.GlobVarDecl ident <$> genExpr value
genDecl (Lfr.GlobFunDecl ident params body) = Anf.GlobFunDecl ident params <$> genExpr body

genExpr :: Lfr.Expression -> AnfGenState Anf.Expression
genExpr (Lfr.ExprId ident) = returnAtom $ Anf.AtomId ident
genExpr (Lfr.ExprPrimVal val) = returnAtom $ case val of
  Common.PrimValUnit -> Anf.AtomUnit
  Common.PrimValBool bool -> Anf.AtomBool bool
  Common.PrimValInt int -> Anf.AtomInt int
genExpr (Lfr.ExprBinOp op lhs rhs) = evalContT $ do
  lhs' <- normalizeToAtom lhs
  rhs' <- normalizeToAtom rhs
  returnComplex $ Anf.CompBinOp op lhs' rhs'
genExpr (Lfr.ExprUnOp op x) = evalContT $ do
  x' <- normalizeToAtom x
  returnComplex $ Anf.CompUnOp op x'
genExpr (Lfr.ExprApp f arg) = evalContT $ do
  f' <- normalizeToId f
  arg' <- normalizeToAtom arg
  returnComplex $ Anf.CompApp f' arg'
genExpr (Lfr.ExprIte c t e) = evalContT $ do
  c' <- normalizeToAtom c
  t' <- lift $ genExpr t
  e' <- lift $ genExpr e
  returnComplex $ Anf.CompIte c' t' e'
genExpr (Lfr.ExprLetIn (Lfr.VarDecl ident val) expr) = do
  val' <- genExpr val
  expr' <- genExpr expr
  return $ Anf.ExprLetIn (ident, val') expr'

returnAtom :: MonadState Env m => Anf.AtomicExpression -> m Anf.Expression
returnAtom = return . Anf.ExprAtom

returnComplex :: MonadState Env m => Anf.ComplexExpression -> m Anf.Expression
returnComplex = return . Anf.ExprComp

-- ** Normalizers

normalizeToAtom :: Lfr.Expression -> NormCont Anf.Expression Anf.AtomicExpression
normalizeToAtom expr = do
  expr' <- lift $ genExpr expr
  case expr' of
    Anf.ExprAtom atom -> return atom
    _ -> do
      ident <- lift genId
      mapContT
        (\e -> Anf.ExprLetIn (ident, expr') <$> e)
        (return $ Anf.AtomId ident)

normalizeToId :: Lfr.Expression -> NormCont Anf.Expression Common.Identifier'
normalizeToId expr = do
  expr' <- lift $ genExpr expr
  case expr' of
    Anf.ExprAtom (Anf.AtomId ident) -> return ident
    _ -> do
      ident <- lift genId
      mapContT
        (\e -> Anf.ExprLetIn (ident, expr') <$> e)
        (return ident)

-- ** Identifier Generation

genId :: AnfGenState Common.Identifier'
genId = do
  cnt <- get
  modify (+ 1)
  return $ Common.Gen cnt $ pack "anf"
