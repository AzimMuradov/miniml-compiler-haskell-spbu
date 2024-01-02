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
        >>> (>>= (generalize >>> fmap fromUPolytype))
        >>> flip runReaderT M.empty
        >>> runExceptT
        >>> evalIntBindingT
        >>> runIdentity

    withStdLib infer = do
      let generalizeDecl (ident, t) = (ident,) <$> generalize (fromTypeToUType t)
      generalizedDecls <- mapM generalizeDecl StdLib.typedDecls
      local (M.union (M.fromList generalizedDecls)) infer

-- * Internal

inferStatements :: [Statement] -> Infer UType
inferStatements x = inferStatements' x (throwError Unreachable)

inferStatements' :: [Statement] -> Infer UType -> Infer UType
inferStatements' [] pr = pr
inferStatements' ((StmtExpr e) : xs) _ = do
  res <- inferExpr e
  inferStatements' xs (return res)
inferStatements' ((StmtDecl (DeclVar (ident, t) body)) : xs) _ = do
  res <- inferExpr body
  vType <- maybe (return res) ((=:=) res <$> fromTypeToUType) t
  pvType <- generalize vType
  withBinding ident pvType (inferStatements' xs $ return vType)
inferStatements' ((StmtDecl (DeclFun ident False fun)) : xs) _ = do
  res <- inferFun fun
  withBinding ident (Forall [] res) (inferStatements' xs $ return res)
inferStatements' ((StmtDecl (DeclFun ident True fun)) : xs) _ = do
  preT <- fresh
  next <- withBinding ident (Forall [] preT) $ inferFun fun
  after <- withBinding ident (Forall [] next) $ inferFun fun
  withBinding ident (Forall [] after) (inferStatements' xs $ return next)

inferExpr :: Expression -> Infer UType
inferExpr (ExprId x) = lookup (Var x)
inferExpr (ExprVal value) = case value of
  ValUnit -> return UTyUnit
  ValBool _ -> return UTyBool
  ValInt _ -> return UTyInt
inferExpr (ExprBinOp op lhs rhs) = do
  utLhs <- inferExpr lhs
  utRhs <- inferExpr rhs
  withError (const $ ImpossibleBinOpApplication utLhs utRhs) $ do
    ut <- utLhs =:= utRhs
    case op of
      BoolOp _ -> ut =:= UTyBool
      ArithOp _ -> ut =:= UTyInt
      CompOp _ -> return UTyBool
inferExpr (ExprUnOp op x) = do
  ut <- inferExpr x
  withError (const $ ImpossibleUnOpApplication ut) $ case op of
    UnMinusOp -> ut =:= UTyInt
inferExpr (ExprApp funExpr argExpr) = do
  funUT <- inferExpr funExpr
  argUT <- inferExpr argExpr
  resUT <- fresh
  _ <- funUT =:= UTyFun argUT resUT
  return resUT
inferExpr (ExprIte c t e) = do
  _ <- check c UTyBool
  t' <- inferExpr t
  e' <- inferExpr e
  t' =:= e'
inferExpr (ExprLetIn decl expr) = inferLetIn decl expr
inferExpr (ExprFun fun) = inferFun fun

inferLetIn :: Declaration -> Expression -> Infer UType
inferLetIn (DeclVar (x, Just pty) xdef) expr = do
  let upty = toUPolytype (Forall [] $ toTypeF pty)
  upty' <- skolemize upty
  bl <- inferExpr xdef
  _ <- bl =:= upty'
  withBinding x upty $ inferExpr expr
inferLetIn (DeclVar (x, Nothing) xdef) expr = do
  ty <- inferExpr xdef
  pty <- generalize ty
  withBinding x pty $ inferExpr expr
inferLetIn (DeclFun f False fun) expr = do
  fdef <- inferFun fun
  pfdef <- generalize fdef
  withBinding f pfdef $ inferExpr expr
inferLetIn (DeclFun f True fun) expr = do
  preT <- fresh
  next <- withBinding f (Forall [] preT) $ inferFun fun
  after <- withBinding f (Forall [] next) $ inferFun fun
  inferredBlock <- withBinding f (Forall [] next) (inferExpr expr)
  pfdef <- generalize after
  withBinding f pfdef (return inferredBlock)

inferFun :: Fun -> Infer UType
inferFun (Fun args restype body) = inferFun' $ toList args
  where
    inferFun' args' = case args' of
      [] -> do
        inferredBody <- inferExpr body
        case restype of
          Just t -> fromTypeToUType t =:= inferredBody
          Nothing -> return inferredBody
      (ident, t) : ys -> do
        t' <- maybe fresh (return . fromTypeToUType) t
        withBinding ident (Forall [] t') $ UTyFun t' <$> inferFun' ys

-- Utils

check :: Expression -> UType -> Infer UType
check e ty = do
  ty' <- inferExpr e
  ty =:= ty'

withError :: MonadError e m => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) pure

tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)
