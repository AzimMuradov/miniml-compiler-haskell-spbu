{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeInference.TypeInference where

import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty, toList)
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
inferSingle (ExprIf conditionExpr thenExpr elseExpr) = do
  _ <- check conditionExpr UTyBool
  thenExpr' <- inferSingle thenExpr
  elseExpr' <- inferSingle elseExpr
  thenExpr' =:= elseExpr'
inferSingle (ExprBinaryOperation op lhs rhs) = do
  lhs' <- inferSingle lhs
  rhs' <- inferSingle rhs
  let opErr = ImpossibleBinOpApplication lhs' rhs'
  ut <- case (lhs', rhs') of
    (UTyFun _ _, _) -> throwError opErr
    (_, UTyFun _ _) -> throwError opErr
    (UTyVar _, _) -> lhs' =:= rhs'
    (_, UTyVar _) -> lhs' =:= rhs'
    _ -> withError (const opErr) (lhs' =:= rhs')
  withError (const opErr) $ case op of
    BooleanOp _ -> ut =:= UTyBool
    ArithmeticOp _ -> ut =:= UTyInt
    ComparisonOp _ -> return UTyBool
inferSingle (ExprUnaryOperation op x) = do
  x' <- inferSingle x
  withError (const $ ImpossibleUnOpApplication x') $ case op of
    NotOp -> x' =:= UTyBool
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

inferFun :: NonEmpty (Identifier, Maybe Type) -> Maybe Type -> Expression -> Infer UType
inferFun args restype body = inferFun' $ toList args
  where
    inferFun' args' = case args' of
      [] -> do
        inferredBody <- inferSingle body
        case restype of
          Just t -> fromTypeToUType t =:= inferredBody
          Nothing -> return inferredBody
      (ident, t) : ys -> do
        t' <- maybe fresh (return . fromTypeToUType) t
        withBinding ident (Forall [] t') $ UTyFun t' <$> inferFun' ys
