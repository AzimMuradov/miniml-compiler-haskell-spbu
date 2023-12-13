{-# LANGUAGE TupleSections #-}

module Transformations.Anf.AnfGen (astToAnf) where

import Control.Monad.Cont (ContT, mapContT)
import Control.Monad.State (MonadTrans (lift), State, evalState, get, modify)
import Control.Monad.Trans.Cont (evalContT)
import Data.Text (pack)
import qualified Parser.Ast as Ast
import qualified Transformations.Anf.Anf as Anf
import Transformations.Cc.Cc (ccAst)
import qualified Transformations.Ll.Lfr as Lfr
import Transformations.Ll.Ll (llAst)
import Transformations.Relabeler.RelabelVars (relabelAst)
import Transformations.Simplification.SimplifyAst (simplifyAst)
import qualified Trees.Common as Common

astToAnf :: Ast.Program -> Anf.Program
astToAnf = lfrToAnf . llAst . ccAst . relabelAst . simplifyAst

lfrToAnf :: Lfr.Program -> Anf.Program
lfrToAnf (Lfr.Program tlDecls cnt) = Anf.Program $ evalState (mapM normalizeDecl tlDecls) cnt

type CpsWithCnt r a = ContT r CntState a

type CntState = State IdCnt

type IdCnt = Int

normalizeDecl :: Lfr.TopLevelDeclaration -> CntState Anf.GlobalDeclaration
normalizeDecl (Lfr.TopLevelVarDecl (Lfr.VarDecl name value)) = Anf.GlobVarDecl name <$> normalizeExpr value
normalizeDecl (Lfr.TopLevelFunDecl name params body) = Anf.GlobFunDecl name params <$> normalizeExpr body

normalizeExpr :: Lfr.Expression -> CntState Anf.Expression
normalizeExpr (Lfr.ExprId name) = returnAtom $ Anf.AtomId name
normalizeExpr (Lfr.ExprVal value) = returnAtom $ case value of
  Common.ValUnit -> Anf.AtomInt 0
  Common.ValBool bool -> Anf.AtomBool bool
  Common.ValInt int -> Anf.AtomInt int
normalizeExpr (Lfr.ExprBinOp op lhs rhs) = evalContT $ do
  lhs' <- normalizeName lhs
  rhs' <- normalizeName rhs
  returnAtom' $ Anf.AtomBinOp op lhs' rhs'
normalizeExpr (Lfr.ExprUnOp op x) = evalContT $ do
  x' <- normalizeName x
  returnAtom' $ Anf.AtomUnOp op x'
normalizeExpr (Lfr.ExprApp f arg) = evalContT $ do
  f' <- normalizeName f
  arg' <- normalizeName arg
  returnComplex $ Anf.CompApp f' arg'
normalizeExpr (Lfr.ExprIte c t e) = evalContT $ do
  c' <- normalizeName c
  t' <- normalizeName t
  e' <- normalizeName e
  returnComplex $ Anf.CompIte c' t' e'
normalizeExpr (Lfr.ExprLetIn decls expr) = do
  decls' <- normalizeVarDecl decls
  expr' <- normalizeExpr expr
  return $ Anf.ExprLetIn decls' expr'
  where
    normalizeVarDecl (Lfr.VarDecl name def) = (name,) <$> normalizeExpr def

returnAtom :: Anf.AtomicExpression -> CntState Anf.Expression
returnAtom = return . Anf.ExprAtom

returnAtom' :: Anf.AtomicExpression -> CpsWithCnt Anf.Expression Anf.Expression
returnAtom' = return . Anf.ExprAtom

returnComplex :: Anf.ComplexExpression -> CpsWithCnt Anf.Expression Anf.Expression
returnComplex = return . Anf.ExprComp

normalizeName :: Lfr.Expression -> CpsWithCnt Anf.Expression Anf.AtomicExpression
normalizeName expr = do
  expr' <- lift $ normalizeExpr expr
  case expr' of
    Anf.ExprAtom atom -> return atom
    _ -> do
      name <- lift genName
      mapContT
        (\e -> Anf.ExprLetIn (name, expr') <$> e)
        (return $ Anf.AtomId name)

genName :: CntState Common.Identifier'
genName = do
  cnt <- get
  modify (+ 1)
  return $ Common.Gen cnt $ pack "anf"
