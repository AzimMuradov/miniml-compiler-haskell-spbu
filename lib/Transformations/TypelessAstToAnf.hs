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
normalizeStatement (StmtDecl name value) = Anf.StmtDecl name <$> normalizeExpr value
normalizeStatement (StmtExpr expr) = Anf.StmtExpr <$> normalizeExpr expr

normalizeExpr :: Expression -> CntState Anf.Expression
normalizeExpr (ExprIdentifier name) = returnAtom $ Anf.AtomIdentifier name
normalizeExpr (ExprValue value) = case value of
  ValUnit -> returnAtom Anf.AtomUnit
  ValBool bool -> returnAtom $ Anf.AtomBool bool
  ValInt int -> returnAtom $ Anf.AtomInt int
  ValFun params body -> Anf.ExprAtom . Anf.AtomClosure params <$> normalizeExpr body
normalizeExpr (ExprApplication f args) = evalContT $ do
  f' <- normalizeName f
  args' <- mapM normalizeName args
  returnComplex $ Anf.CompApp f' args'
normalizeExpr (ExprIte c t e) = evalContT $ do
  c' <- normalizeName c
  t' <- lift $ normalizeExpr t
  e' <- lift $ normalizeExpr e
  returnComplex $ Anf.CompIte c' t' e'
normalizeExpr (ExprLetIn (name, value) expr) = do
  value' <- normalizeExpr value
  expr' <- normalizeExpr expr
  return $ Anf.ExprLetIn name value' expr'

returnAtom :: Anf.AtomicExpression -> CntState Anf.Expression
returnAtom = return . Anf.ExprAtom

returnComplex :: Anf.ComplexExpression -> CpsWithCnt Anf.Expression Anf.Expression
returnComplex = return . Anf.ExprComp

normalizeName :: Expression -> CpsWithCnt Anf.Expression Anf.AtomicExpression
normalizeName expr = do
  expr' <- lift $ normalizeExpr expr
  case expr' of
    Anf.ExprAtom atom -> return atom
    _ -> do
      name <- genName
      mapContT
        (\e -> Anf.ExprLetIn name expr' <$> e)
        (return $ Anf.AtomIdentifier name)

genName :: CpsWithCnt r Text
genName = do
  n <- get
  modify (+ 1)
  return $ cons '$' (pack $ show n)
