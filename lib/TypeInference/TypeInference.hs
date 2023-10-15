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

check :: Expression -> UType -> Infer UType
check e ty = do
  ty' <- inferSingle e
  ty =:= ty'

helpInferStatements :: [Statement] -> Infer UType -> Infer UType
helpInferStatements [] pr = pr
helpInferStatements ((StmtVarDecl (VarDecl (ident, t) body)) : xs) _ = do
  _ <- checkForDuplicate (Var ident)
  res <- inferSingle body
  vType <- maybe (return res) ((=:=) res <$> fromTypeToUType) t
  pvType <- generalize vType
  withBinding ident pvType (helpInferStatements xs $ return vType)
helpInferStatements ((StmtFunDecl (FunDecl ident (Fun args restype body))) : xs) _ = do
  _ <- checkForDuplicate (Var ident)
  res <- inferFun args restype body
  withBinding ident (Forall [] res) (helpInferStatements xs $ return res)
helpInferStatements ((StmtRecFunDecl (RecFunDecl ident (Fun args restype body))) : xs) _ = do
  _ <- checkForDuplicate (Var ident)
  preT <- fresh
  next <- withBinding ident (Forall [] preT) $ inferFun args restype body
  after <- withBinding ident (Forall [] next) $ inferFun args restype body
  withBinding ident (Forall [] after) (helpInferStatements xs $ return next)
helpInferStatements ((StmtExpr e) : xs) _ = do
  res <- inferSingle e
  helpInferStatements xs (return res)

inferStatement :: [Statement] -> Infer UType
inferStatement x = helpInferStatements x (throwError EmptyList)

inferSingle :: Expression -> Infer UType
inferSingle (ExprIdentifier x) = lookup (Var x)
inferSingle (ExprValue (ValBool _)) = return UTyBool
inferSingle (ExprValue (ValInt _)) = return UTyInt
inferSingle (ExprValue (ValFun (Fun xs restype body))) = inferFun xs restype body
inferSingle (ExprIf e1 e2 e3) = do
  _ <- check e1 UTyBool
  e2' <- inferSingle e2
  e3' <- inferSingle e3
  e2' =:= e3'
inferSingle (ExprOperations (NotOp x)) = do
  _ <- check x UTyBool
  return UTyBool
inferSingle (ExprOperations (BooleanOp x)) = booleanOpInfer (bL x) (bR x)
inferSingle (ExprOperations (ComparisonOp x)) = comparationOpInfer (cL x) (cR x)
inferSingle (ExprOperations (ArithmeticOp x)) = arithmeticOperationInfer (aL x) (aR x)
inferSingle (ExprLetInV (x, Just pty) xdef body) = do
  let upty = toUPolytype (Forall [] $ toTypeF pty)
  upty' <- skolemize upty
  bl <- inferSingle xdef
  _ <- bl =:= upty'
  withBinding x upty $ inferSingle body
inferSingle (ExprLetInV (x, Nothing) xdef body) = do
  ty <- inferSingle xdef
  pty <- generalize ty
  withBinding x pty $ inferSingle body
inferSingle (ExprLetInF f (Fun args restype fbody) lbody) = do
  fdef <- inferFun args restype fbody
  pfdef <- generalize fdef
  withBinding f pfdef $ inferSingle lbody
inferSingle (ExprApplication e1 e2) = do
  funTy <- inferSingle e1
  argTy <- inferSingle e2
  resTy <- fresh
  _ <- funTy =:= UTyFun argTy resTy
  return resTy
inferSingle (ExprLetRecInF f (Fun args restype fbody) lbody) = do
  preT <- fresh
  next <- withBinding f (Forall [] preT) $ inferFun args restype fbody
  after <- withBinding f (Forall [] next) $ inferFun args restype fbody
  inferedBlock <- withBinding f (Forall [] next) (inferSingle lbody)
  pfdef <- generalize after
  withBinding f pfdef (return inferedBlock)

arithmeticOperationInfer :: Expression -> Expression -> Infer UType
arithmeticOperationInfer e1 e2 = do
  t1 <- inferSingle e1
  t2 <- inferSingle e2
  case (t1, t2) of
    (UVar _, UVar _) -> (t1 =:= UTyInt) <* (t2 =:= UTyInt)
    (UTyInt, _) -> t1 =:= t2
    (_, UTyInt) -> t1 =:= t2
    _ -> throwError $ ImpossibleOpApplication t1 t2

booleanOpInfer :: Expression -> Expression -> Infer UType
booleanOpInfer e1 e2 = do
  t1 <- inferSingle e1
  t2 <- inferSingle e2
  case (t1, t2) of
    (UTyBool, UTyBool) -> return UTyBool
    _ -> throwError $ ImpossibleOpApplication t1 t2

comparationOpInfer :: Expression -> Expression -> Infer UType
comparationOpInfer e1 e2 = do
  t1 <- inferSingle e1
  t2 <- inferSingle e2
  _ <- t1 =:= t2
  return UTyBool

inferFun :: [(Identifier, Maybe Type)] -> Maybe Type -> Expression -> Infer UType
inferFun args restype body = case args of
  [] -> do
    inferedBody <- inferSingle body
    case restype of
      (Just x) -> fromTypeToUType x =:= inferedBody
      Nothing -> return inferedBody
  ((ident, Just t) : ys) ->
    let ut = fromTypeToUType t
     in withBinding ident (Forall [] ut) $ UTyFun ut <$> inferFun ys restype body
  ((ident, Nothing) : ys) -> do
    argTy <- fresh
    withBinding ident (Forall [] argTy) $ UTyFun argTy <$> inferFun ys restype body