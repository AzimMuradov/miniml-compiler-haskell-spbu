module Transformations.TypelessAstToAnf (typelessAstToAnf) where

import Control.Monad.Cont (ContT, evalContT, mapContT)
import Control.Monad.State (MonadTrans (lift), State, evalState, get, modify)
import Data.Text (Text, cons, pack)
import qualified Transformations.Anf as Anf
import Transformations.TypelessAst

typelessAstToAnf :: Program -> Anf.Program
typelessAstToAnf program = evalState (normalizeProgram program) 0

type CpsWithCnt r a = ContT r CntState a

type CntState = State Int

normalizeProgram :: Program -> CntState Anf.Program
normalizeProgram (Program stmts) = Anf.Program <$> mapM normalizeStatement stmts

normalizeStatement :: Statement -> CntState Anf.Statement
normalizeStatement (StmtDecl name value) = Anf.StmtDecl name <$> normalizeTerm' value
normalizeStatement (StmtExpr expr) = Anf.StmtExpr <$> normalizeTerm' expr

normalizeExpr :: Expression -> CpsWithCnt Anf.Expression Anf.Expression
normalizeExpr (ExprIdentifier name) = return $ Anf.ExprAtomExpr $ Anf.AtomExprIdentifier name
normalizeExpr (ExprValue value) = case value of
  ValUnit -> return $ Anf.ExprAtomExpr Anf.AtomExprUnit
  ValBool bool -> return $ Anf.ExprAtomExpr $ Anf.AtomExprBool bool
  ValInt int -> return $ Anf.ExprAtomExpr $ Anf.AtomExprInt int
  ValFun params body -> Anf.ExprAtomExpr <$> (Anf.AtomExprClosure params <$> normalizeTerm body)
normalizeExpr (ExprApplication f args) = Anf.ExprCompExpr <$> (Anf.CompExprApp <$> normalizeName f <*> mapM normalizeName args)
normalizeExpr (ExprIte c t e) = do
  c' <- normalizeName c
  t' <- normalizeTerm t
  e' <- normalizeTerm e
  return $ Anf.ExprCompExpr $ Anf.CompExprIte c' t' e'
normalizeExpr (ExprLetIn x value expr) = Anf.ExprLetIn x <$> normalizeExpr value <*> normalizeExpr expr

normalizeTerm :: Expression -> CpsWithCnt r Anf.Expression
normalizeTerm expr = lift $ normalizeTerm' expr

normalizeTerm' :: Expression -> CntState Anf.Expression
normalizeTerm' expr = evalContT $ normalizeExpr expr

normalizeName :: Expression -> CpsWithCnt Anf.Expression Anf.AtomicExpression
normalizeName expr = do
  expr' <- normalizeExpr expr
  case expr' of
    Anf.ExprAtomExpr atom -> return atom
    _ -> do
      name <- genName
      mapContT
        (\expr'' -> Anf.ExprLetIn name expr' <$> expr'')
        (return $ Anf.AtomExprIdentifier name)

genName :: CpsWithCnt r Text
genName = do
  n <- get
  modify (+ 1)
  return $ cons '$' (pack $ show n)
