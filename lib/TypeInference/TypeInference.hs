{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TypeInference.TypeInference where

import Control.Monad.Except
import Data.List.NonEmpty (toList)
import Data.Maybe
import Parser.Ast
import TypeInference.HindleyMilner
import Prelude hiding (lookup)

check :: Expression -> UType -> Infer UType
check e ty = do
  ty' <- inferSingle e
  ty =:= ty'

inferStatements :: [Statement] -> Infer UType
inferStatements x = inferStatements' x (throwError Unreachable)

inferStatements' :: [Statement] -> Infer UType -> Infer UType
inferStatements' [] pr = pr
inferStatements' ((StmtExpr e) : xs) _ = do
  res <- inferSingle e
  inferStatements' xs (return res)
inferStatements' ((StmtUserDecl (DeclVar (ident, t) body)) : xs) _ = do
  res <- inferSingle body
  vType <- maybe (return res) ((=:=) res <$> fromTypeToUType) t
  pvType <- generalize vType
  withBinding ident pvType (inferStatements' xs $ return vType)
inferStatements' ((StmtUserDecl (DeclFun ident fun)) : xs) _ = do
  res <- inferFun fun
  withBinding ident (Forall [] res) (inferStatements' xs $ return res)
inferStatements' ((StmtUserDecl (DeclRecFun ident fun)) : xs) _ = do
  preT <- fresh
  next <- withBinding ident (Forall [] preT) $ inferFun fun
  after <- withBinding ident (Forall [] next) $ inferFun fun
  withBinding ident (Forall [] after) (inferStatements' xs $ return next)
inferStatements' ((StmtStdDecl (StdDecl ident t)) : xs) _ = do
  let t' = fromTypeToUType t
  pT <- generalize t'
  withBinding ident pT (inferStatements' xs $ return t')

inferSingle :: Expression -> Infer UType
inferSingle (ExprIdentifier x) = lookup (Var x)
inferSingle (ExprValue value) = case value of
  ValUnit -> return UTyUnit
  ValBool _ -> return UTyBool
  ValInt _ -> return UTyInt
  ValFun fun -> inferFun fun
inferSingle (ExprBinaryOperation op lhs rhs) = do
  utLhs <- inferSingle lhs
  utRhs <- inferSingle rhs
  withError (const $ ImpossibleBinOpApplication utLhs utRhs) $ do
    ut <- utLhs =:= utRhs
    case op of
      BooleanOp _ -> ut =:= UTyBool
      ArithmeticOp _ -> ut =:= UTyInt
      ComparisonOp _ -> return UTyBool
inferSingle (ExprUnaryOperation op x) = do
  ut <- inferSingle x
  withError (const $ ImpossibleUnOpApplication ut) $ case op of
    UnaryMinusOp -> ut =:= UTyInt
inferSingle (ExprApplication e1 e2) = do
  funTy <- inferSingle e1
  argTy <- inferSingle e2
  resTy <- fresh
  _ <- funTy =:= UTyFun argTy resTy
  return resTy
inferSingle (ExprIf e1 e2 e3) = do
  _ <- check e1 UTyBool
  e2' <- inferSingle e2
  e3' <- inferSingle e3
  e2' =:= e3'
inferSingle (ExprLetIn (DeclVar (x, Just pty) xdef) body) = do
  let upty = toUPolytype (Forall [] $ toTypeF pty)
  upty' <- skolemize upty
  bl <- inferSingle xdef
  _ <- bl =:= upty'
  withBinding x upty $ inferSingle body
inferSingle (ExprLetIn (DeclVar (x, Nothing) xdef) body) = do
  ty <- inferSingle xdef
  pty <- generalize ty
  withBinding x pty $ inferSingle body
inferSingle (ExprLetIn (DeclFun f fun) lbody) = do
  fdef <- inferFun fun
  pfdef <- generalize fdef
  withBinding f pfdef $ inferSingle lbody
inferSingle (ExprLetIn (DeclRecFun f fun) lbody) = do
  preT <- fresh
  next <- withBinding f (Forall [] preT) $ inferFun fun
  after <- withBinding f (Forall [] next) $ inferFun fun
  inferedBlock <- withBinding f (Forall [] next) (inferSingle lbody)
  pfdef <- generalize after
  withBinding f pfdef (return inferedBlock)

inferFun :: Fun -> Infer UType
inferFun (Fun args restype body) = inferFun' $ toList args
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
