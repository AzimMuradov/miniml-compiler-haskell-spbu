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
  ty' <- inferExpression e
  ty =:= ty'

inferStatements :: [Statement] -> Infer UType
inferStatements x = inferStatements' x (throwError Unreachable)

inferStatements' :: [Statement] -> Infer UType -> Infer UType
inferStatements' [] pr = pr
inferStatements' ((StmtExpr e) : xs) _ = do
  res <- inferExpression e
  inferStatements' xs (return res)
inferStatements' ((StmtUserDecl (DeclVar (ident, t) body)) : xs) _ = do
  res <- inferExpression body
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

inferExpression :: Expression -> Infer UType
inferExpression (ExprIdentifier x) = lookup (Var x)
inferExpression (ExprValue value) = case value of
  ValUnit -> return UTyUnit
  ValBool _ -> return UTyBool
  ValInt _ -> return UTyInt
  ValFun fun -> inferFun fun
inferExpression (ExprBinaryOperation op lhs rhs) = do
  utLhs <- inferExpression lhs
  utRhs <- inferExpression rhs
  withError (const $ ImpossibleBinOpApplication utLhs utRhs) $ do
    ut <- utLhs =:= utRhs
    case op of
      BooleanOp _ -> ut =:= UTyBool
      ArithmeticOp _ -> ut =:= UTyInt
      ComparisonOp _ -> return UTyBool
inferExpression (ExprUnaryOperation op x) = do
  ut <- inferExpression x
  withError (const $ ImpossibleUnOpApplication ut) $ case op of
    UnaryMinusOp -> ut =:= UTyInt
inferExpression (ExprApplication funExpr argExpr) = do
  funUT <- inferExpression funExpr
  argUT <- inferExpression argExpr
  resUT <- fresh
  _ <- funUT =:= UTyFun argUT resUT
  return resUT
inferExpression (ExprIf c t e) = do
  _ <- check c UTyBool
  t' <- inferExpression t
  e' <- inferExpression e
  t' =:= e'
inferExpression (ExprLetIn decl expr) = inferLetIn decl expr

inferLetIn :: UserDeclaration -> Expression -> Infer UType
inferLetIn (DeclVar (x, Just pty) xdef) expr = do
  let upty = toUPolytype (Forall [] $ toTypeF pty)
  upty' <- skolemize upty
  bl <- inferExpression xdef
  _ <- bl =:= upty'
  withBinding x upty $ inferExpression expr
inferLetIn (DeclVar (x, Nothing) xdef) expr = do
  ty <- inferExpression xdef
  pty <- generalize ty
  withBinding x pty $ inferExpression expr
inferLetIn (DeclFun f fun) expr = do
  fdef <- inferFun fun
  pfdef <- generalize fdef
  withBinding f pfdef $ inferExpression expr
inferLetIn (DeclRecFun f fun) expr = do
  preT <- fresh
  next <- withBinding f (Forall [] preT) $ inferFun fun
  after <- withBinding f (Forall [] next) $ inferFun fun
  inferredBlock <- withBinding f (Forall [] next) (inferExpression expr)
  pfdef <- generalize after
  withBinding f pfdef (return inferredBlock)

inferFun :: Fun -> Infer UType
inferFun (Fun args restype body) = inferFun' $ toList args
  where
    inferFun' args' = case args' of
      [] -> do
        inferredBody <- inferExpression body
        case restype of
          Just t -> fromTypeToUType t =:= inferredBody
          Nothing -> return inferredBody
      (ident, t) : ys -> do
        t' <- maybe fresh (return . fromTypeToUType) t
        withBinding ident (Forall [] t') $ UTyFun t' <$> inferFun' ys
