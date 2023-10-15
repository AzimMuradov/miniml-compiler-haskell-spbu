{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeInference.TypeInference where

import Control.Monad.Except
import Control.Unification (UTerm (UVar))
import Data.Maybe
import Parser.Ast
import TypeInference.HindleyMilner
import Prelude hiding (lookup)

check :: Expr -> UType -> Infer UType
check e ty = do
  ty' <- inferSingle e
  ty =:= ty'

helpInferStatements :: [Statement] -> Infer UType -> Infer UType
helpInferStatements [] pr = pr
helpInferStatements ((SVarDecl (VarDecl (ident, t) body)) : xs) _ = do
  _ <- checkForDuplicate (Var ident)
  res <- inferBlock body
  vType <- maybe (return res) ((=:=) res <$> fromTypeToUType) t
  pvType <- generalize vType
  withBinding ident pvType (helpInferStatements xs $ return vType)
helpInferStatements ((SFunDecl (FunDecl ident (Fun args body))) : xs) _ = do
  _ <- checkForDuplicate (Var ident)
  res <- inferFun args body
  withBinding ident (Forall [] res) (helpInferStatements xs $ return res)
helpInferStatements ((SRecFunDecl (RecFunDecl ident (Fun args body))) : xs) _ = do
  _ <- checkForDuplicate (Var ident)
  preT <- fresh
  next <- withBinding ident (Forall [] preT) $ inferFun args body
  after <- withBinding ident (Forall [] next) $ inferFun args body
  withBinding ident (Forall [] after) (helpInferStatements xs $ return next)
helpInferStatements ((SExpr e) : xs) _ = do
  res <- inferSingle e
  helpInferStatements xs (return res)

inferStatement :: [Statement] -> Infer UType
inferStatement x = helpInferStatements x (throwError EmptyList)

inferBlock :: [Expr] -> Infer UType
inferBlock [] = throwError EmptyList
inferBlock [x] = inferSingle x
inferBlock (x : xs) = inferSingle x >> inferBlock xs

inferSingle :: Expr -> Infer UType
inferSingle (EIdentifier x) = lookup (Var x)
inferSingle (EValue (VBool _)) = return UTyBool
inferSingle (EValue (VInt _)) = return UTyInt
inferSingle (EValue (VFun (Fun xs body))) = inferFun xs body
inferSingle (EIf e1 e2 e3) = do
  _ <- check e1 UTyBool
  e2' <- inferBlock e2
  e3' <- inferBlock e3
  e2' =:= e3'
inferSingle (EOperations (NotOp x)) = do
  _ <- check x UTyBool
  return UTyBool
inferSingle (EOperations (BooleanOp x)) = booleanOpInfer (bL x) (bR x)
inferSingle (EOperations (ComparisonOp x)) = comparationOpInfer (cL x) (cR x)
inferSingle (EOperations (ArithmeticOp x)) = arithmeticOperationInfer (aL x) (aR x)
inferSingle (ELetInV (x, Just pty) xdef body) = do
  let upty = toUPolytype (Forall [] $ toTypeF pty)
  upty' <- skolemize upty
  bl <- inferBlock xdef
  _ <- bl =:= upty'
  withBinding x upty $ inferBlock body
inferSingle (ELetInV (x, Nothing) xdef body) = do
  ty <- inferBlock xdef
  pty <- generalize ty
  withBinding x pty $ inferBlock body
inferSingle (ELetInF f (Fun args fbody) lbody) = do
  fdef <- inferFun args fbody
  pfdef <- generalize fdef
  withBinding f pfdef $ inferBlock lbody
inferSingle (EApplication e1 e2) = do
  funTy <- inferSingle e1
  argTy <- inferSingle e2
  resTy <- fresh
  _ <- funTy =:= UTyFun argTy resTy
  return resTy
inferSingle (ELetRecInF f (Fun args fbody) lbody) = do
  preT <- fresh
  next <- withBinding f (Forall [] preT) $ inferFun args fbody
  after <- withBinding f (Forall [] next) $ inferFun args fbody
  inferedBlock <- withBinding f (Forall [] next) (inferBlock lbody)
  pfdef <- generalize after
  withBinding f pfdef (return inferedBlock)

arithmeticOperationInfer :: Expr -> Expr -> Infer UType
arithmeticOperationInfer e1 e2 = do
  t1 <- inferSingle e1
  t2 <- inferSingle e2
  case (t1, t2) of
    (UVar _, UVar _) -> t1 =:= t2
    (UTyInt, _) -> t1 =:= t2
    (_, UTyInt) -> t1 =:= t2
    _ -> throwError $ ImpossibleOpApplication t1 t2

booleanOpInfer :: Expr -> Expr -> Infer UType
booleanOpInfer e1 e2 = do
  t1 <- inferSingle e1
  t2 <- inferSingle e2
  case (t1, t2) of
    (UTyBool, UTyBool) -> return UTyBool
    _ -> throwError $ ImpossibleOpApplication t1 t2

comparationOpInfer :: Expr -> Expr -> Infer UType
comparationOpInfer e1 e2 = do
  t1 <- inferSingle e1
  t2 <- inferSingle e2
  _ <- t1 =:= t2
  return UTyBool

inferFun :: [(Identifier, Maybe Type)] -> [Expr] -> Infer UType
inferFun args body = case args of
  [] -> inferBlock body
  ((ident, Just t) : ys) ->
    let ut = fromTypeToUType t
     in withBinding ident (Forall [] ut) $ UTyFun ut <$> inferFun ys body
  ((ident, Nothing) : ys) -> do
    argTy <- fresh
    withBinding ident (Forall [] argTy) $ UTyFun argTy <$> inferFun ys body
