{-# LANGUAGE TupleSections #-}

module TypeChecker.TypeChecker (inferProgram, checkProgram) where

import Control.Category ((>>>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Unification.IntVar (evalIntBindingT)
import Data.Either.Extra (mapRight)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List.NonEmpty (toList)
import qualified Data.Map as M
import Data.Maybe
import Parser.Ast
import qualified StdLib
import Trees.Common
import qualified Trees.Common as L
import TypeChecker.HindleyMilner
import Prelude hiding (lookup)

-- * Program Type Checker

checkProgram :: Program -> Either TypeError ()
checkProgram = mapRight (const ()) . inferProgram

inferProgram :: Program -> Either TypeError Polytype
inferProgram (Program stmts) = runInfer $ withStdLib (inferStatements stmts)
  where
    runInfer :: Infer UType -> Either TypeError Polytype
    runInfer =
      (>>= applyBindings)
        >>> (>>= (generalize >>> fmap toPolytype))
        >>> flip runReaderT M.empty
        >>> runExceptT
        >>> evalIntBindingT
        >>> runIdentity

    withStdLib infer = do
      let generalizeDecl (ident, t) = (ident,) <$> generalize (toUType t)
      generalizedDecls <- mapM generalizeDecl StdLib.typedDecls
      local (M.union (M.fromList generalizedDecls)) infer

-- * Internal

inferStatements :: [Statement] -> Infer UType
inferStatements x = inferStatements' x (throwError Unreachable)

inferStatements' :: [Statement] -> Infer UType -> Infer UType
inferStatements' [] t = t
inferStatements' ((StmtExpr e) : stmts) _ = do
  res <- inferExpr e
  inferStatements' stmts (return res)
inferStatements' ((StmtDecl (DeclVar (ident, t) val)) : stmts) _ = do
  t' <- inferExpr val
  t'' <- checkByAnnotation t' t
  upt <- generalize t''
  withBinding ident upt $ inferStatements' stmts (return t'')
inferStatements' ((StmtDecl (DeclFun ident isRec fun)) : stmts) _ = do
  funT <-
    if isRec
      then do
        funT <- fresh
        funT' <- withBinding ident (Forall [] funT) $ inferFun fun
        withBinding ident (Forall [] funT') $ inferFun fun
      else inferFun fun
  funUT <- generalize funT
  withBinding ident funUT $ inferStatements' stmts (return funT)

inferExpr :: Expression -> Infer UType
inferExpr (ExprId ident) = lookup ident
inferExpr (ExprPrimVal value) = case value of
  PrimValUnit -> return UTUnit
  PrimValBool _ -> return UTBool
  PrimValInt _ -> return UTInt
inferExpr (ExprBinOp op lhs rhs) = do
  lhsT <- inferExpr lhs
  rhsT <- inferExpr rhs
  withError (const $ ImpossibleBinOpApplication lhsT rhsT) $ do
    valT <- lhsT =:= rhsT
    case op of
      BoolOp _ -> valT =:= UTBool
      ArithOp _ -> valT =:= UTInt
      CompOp _ -> return UTBool
inferExpr (ExprUnOp op val) = do
  valT <- inferExpr val
  withError (const $ ImpossibleUnOpApplication valT) $ case op of
    UnMinusOp -> valT =:= UTInt
inferExpr (ExprApp funExpr argExpr) = do
  funT <- inferExpr funExpr
  argT <- inferExpr argExpr
  resT <- fresh
  _ <- funT =:= UTFun argT resT
  return resT
inferExpr (ExprIte c t e) = do
  _ <- check c UTBool
  tT <- inferExpr t
  eT <- inferExpr e
  tT =:= eT
inferExpr (ExprLetIn decl expr) = inferLetIn decl expr
inferExpr (ExprFun fun) = inferFun fun

inferLetIn :: Declaration -> Expression -> Infer UType
inferLetIn (DeclVar (ident, t) val) expr = do
  t' <- inferExpr val
  t'' <- checkByAnnotation t' t
  upt <- generalize t''
  withBinding ident upt $ inferExpr expr
inferLetIn (DeclFun ident isRec fun) expr = do
  funT <-
    if isRec
      then do
        funT <- fresh
        funT' <- withBinding ident (Forall [] funT) $ inferFun fun
        withBinding ident (Forall [] funT') $ inferFun fun
      else inferFun fun
  funUT <- generalize funT
  withBinding ident funUT $ inferExpr expr

inferFun :: Fun -> Infer UType
inferFun (Fun params resT body) = inferFun' $ toList params
  where
    inferFun' params' = case params' of
      [] -> do
        bodyT <- inferExpr body
        checkByAnnotation bodyT resT
      (ident, t) : params'' -> do
        t' <- maybe fresh (return . toUType) t
        withBinding ident (Forall [] t') $ UTFun t' <$> inferFun' params''

-- ** Utils

check :: Expression -> UType -> Infer UType
check expr t = do
  exprT <- inferExpr expr
  t =:= exprT

checkByAnnotation :: UType -> Maybe L.Type -> Infer UType
checkByAnnotation t ann = case ann of
  Just annT -> toUType annT =:= t
  Nothing -> return t
